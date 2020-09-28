------------------------------------------------------------------------------
--                                                                          --
--                                  UWrap                                   --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- UWrap is free software;  you can  redistribute it  and/or modify it      --
-- under terms of the  GNU General Public License as published  by the Free --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version.  UWrap is distributed in the hope that it will be useful, but   --
-- WITHOUT ANY WARRANTY;  without even the  implied warranty of MERCHANTA-  --
-- BILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public  --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General Public License  distributed with UWrap; see file COPYING3.  If   --
-- not, go to http://www.gnu.org/licenses for a complete copy of the        --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Containers;              use Ada.Containers;

with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;
with Wrapping.Runtime.Nodes;   use Wrapping.Runtime.Nodes;
with Wrapping.Utils;           use Wrapping.Utils;

package body Wrapping.Runtime.Frames is

   Data_Frame_Stack : Data_Frame_Vectors.Vector;
   --  Stores the frames as they're pushed and pop during the evaluation of
   --  the program.

   procedure Update_Top_Object;
   --  Updates the global reference to the top object, called when an object is
   --  pushed or popped.

   procedure Update_Top_And_Parent_Frames;
   --  Updates the global references to top and parent frames, called when
   --  a framed is pushed or popped.

   -----------------
   --  Call_Yield --
   -----------------

   procedure Call_Yield
     (Callback : Yield_Callback_Type := Top_Context.Yield_Callback)
   is
   begin
      if Callback /= null then
         Push_Frame_Context;

         --  Yield is not transitive. For example in something like:
         --     child ().filter (condition)
         --  child will values to filter, calling the callback on the
         --  condition. That condition should not be yeilding.

         Top_Context.Yield_Callback := null;

         Callback.all;

         --  A yield callback is supposed to push its result on the stack.
         --  Call_Yield semantic is to replace the current top by the new one
         --  coming from the callback, so pops the previous top.

         Pop_Underneath_Top;

         Pop_Frame_Context;
      end if;
   end Call_Yield;

   ------------------------
   -- Get_Visible_Symbol --
   ------------------------

   function Get_Local_Symbol (Name : Text_Type) return W_Object is
   begin
      if Top_Frame.Symbols.Contains (Name) then
         return Top_Frame.Symbols.Element (Name);
      end if;

      return null;
   end Get_Local_Symbol;

   ----------------
   -- Get_Module --
   ----------------

   function Get_Module
     (A_Frame : Data_Frame_Type) return T_Module
   is
      Scope : T_Entity := A_Frame.Lexical_Scope;
   begin
      --  Look for the current scope, which could be any lexical entity
      --  containing other entities (e.g. a command, a template, a function...)
      --  Look for the parent up until we reach the module.

      while Scope /= null and then Scope.all not in T_Module_Type'Class loop
         Scope := Scope.Parent;
      end loop;

      return T_Module (Scope);
   end Get_Module;

   -----------------------
   -- Update_Top_Object --
   -----------------------

   procedure Update_Top_Object is
   begin
      if Top_Frame /= null and then Top_Frame.Data_Stack.Length > 0 then
         Top_Object_Ref := Top_Frame.Data_Stack.Last_Element;
      end if;
   end Update_Top_Object;

   -----------------
   -- Push_Object --
   -----------------

   procedure Push_Object (Object : access W_Object_Type'Class) is
   begin
      --  We should never push a reference pointing to null. Check that it's
      --  indeed the case.

      pragma Assert (if Object.all in W_Reference_Type'Class then
                        W_Reference (Object).Value /= null);

      Top_Frame.Data_Stack.Append (W_Object (Object));
      Update_Top_Object;
   end Push_Object;

   ----------------------
   -- Push_Implicit_It --
   ----------------------

   procedure Push_Implicit_It (Object : access W_Object_Type'Class) is
   begin
      Top_Context.It_Value := W_Object (Object);
      Push_Object (Top_Context.It_Value);
   end Push_Implicit_It;

   ---------------------------
   -- Push_Allocated_Entity --
   ---------------------------

   procedure Push_Allocated_Entity (Object : access W_Object_Type'Class) is
   begin
      Push_Object
        (W_Object'
           (new W_Reference_Type'
                (Value        => W_Object (Object),
                 Is_Allocated => True)));
   end Push_Allocated_Entity;

   -------------------------
   -- Push_Temporary_Name --
   -------------------------

   procedure Push_Temporary_Name (Name : Text_Type; Object : W_Object)
   is
      Node : constant W_Node := W_Node (Object);
      Node_Map : Text_Maps_Access;
   begin
      --  Checks if the frame already has temporaries for the object in
      --  parameter. Either retreives the corresponding set of names or create
      --  one

      if Top_Frame.Temp_Names.Contains (Object) then
         Node_Map := Top_Frame.Temp_Names.Element (Object);
      else
         Node_Map := new Text_Maps.Map;
         Top_Frame.Temp_Names.Insert (Object, Node_Map);
      end if;

      --  Check if the list of names already has a temporary for the name in
      --  parameter. If yes, push it, if not, increment the tmp counter for
      --  this object and push the temporary.

      if Node_Map.Contains (Name) then
         Push_Object (To_W_String (Node_Map.Element (Name)));
      else
         Node.Tmp_Counter := Node.Tmp_Counter + 1;

         declare
            Tmp : constant Text_Type :=
              "Temp_" & (if Name /= "" then Name & "_" else "") &
              Trim (Integer'Wide_Wide_Image (Node.Tmp_Counter), Both);
         begin
            Node_Map.Insert (Name, Tmp);

            Push_Object (To_W_String (Tmp));
         end;
      end if;
   end Push_Temporary_Name;

   ----------------
   -- Pop_Object --
   ----------------

   procedure Pop_Object is
   begin
      Top_Frame.Data_Stack.Delete_Last;
      Update_Top_Object;
   end Pop_Object;

   ------------------------
   -- Pop_Underneath_Top --
   ------------------------

   procedure Pop_Underneath_Top is
   begin
      Top_Frame.Data_Stack.Delete (Integer (Top_Frame.Data_Stack.Length) - 1);
      Update_Top_Object;
   end Pop_Underneath_Top;

   ----------------
   -- Pop_Object --
   ----------------

   function Pop_Object return W_Object is
      Result : W_Object;
   begin
      Result := Top_Frame.Data_Stack.Last_Element;
      Pop_Object;
      Update_Top_Object;

      return Result;
   end Pop_Object;

   ------------------------
   -- Push_Frame_Context --
   ------------------------

   procedure Push_Frame_Context is
   begin
      Push_Frame_Context (Top_Context.all);
   end Push_Frame_Context;

   ----------------------------------
   -- Push_Frame_Context_Parameter --
   ----------------------------------

   procedure Push_Frame_Context_Parameter is
   begin
      --  When pushing a context with a regular parameter, we're not supposed
      --  to match its result with anything, and the expression as a whole
      --  is a root selection (ie it has no prefix).

      Push_Frame_Context_No_Match;
      Top_Context.Is_Root_Selection := True;
   end Push_Frame_Context_Parameter;

   ---------------------------------------------
   -- Push_Frame_Context_Parameter_With_Match --
   ---------------------------------------------

   procedure Push_Frame_Context_Parameter_With_Match (Object : W_Object) is
   begin
      --  When pushing a context with a parameter to be matched, we need
      --  to set the mode as the default match for a reference (it will be
      --  changed by the expression if needed), set the action to be a match
      --  against the object in parameter. The expression is a root expression
      --  (it has no prefix).

      Push_Frame_Context;
      Top_Context.Is_Root_Selection := True;
      Top_Context.Match_Mode        := Match_Ref_Default;
      Top_Context.Outer_Expr_Action := Action_Match;
      Top_Context.Outer_Object      := Object;
   end Push_Frame_Context_Parameter_With_Match;

   ---------------------------------
   -- Push_Frame_Context_No_Outer --
   ---------------------------------

   procedure Push_Frame_Context_No_Outer is
   begin
      --  Remove all data used for the outer actions (the result callback,
      --  expr action and outer object). Match_Mode is ketp unchanged, as it's
      --  also used to control wether a reference not found is an error or if
      --  it just signals a expression not matching.

      Push_Frame_Context;
      Top_Context.Function_Result_Callback := null;
      Top_Context.Outer_Expr_Action        := Action_None;
      Top_Context.Outer_Object             := null;
   end Push_Frame_Context_No_Outer;

   ---------------------------------
   -- Push_Frame_Context_No_Match --
   ---------------------------------

   procedure Push_Frame_Context_No_Match is
   begin
      --  Sets a frame context as not being matching - there's no outer action,
      --  no match mode and no match object.

      Push_Frame_Context;
      Top_Context.Match_Mode        := Match_None;
      Top_Context.Outer_Expr_Action := Action_None;
      Top_Context.Outer_Object      := null;
   end Push_Frame_Context_No_Match;

   --------------------------------
   -- Push_Frame_Context_No_Pick --
   --------------------------------

   procedure Push_Frame_Context_No_Pick is
   begin
      --  Set a frame with no pick action, that is to say no call to function
      --  after processing and no outer pick action.

      Push_Frame_Context;
      Top_Context.Function_Result_Callback := null;
      Top_Context.Outer_Expr_Action        := Action_None;
   end Push_Frame_Context_No_Pick;

   ------------------------
   -- Push_Frame_Context --
   ------------------------

   procedure Push_Frame_Context (Context : Frame_Context_Type) is
      Parent : constant Frame_Context := Top_Context;
   begin
      Top_Frame.Top_Context := new Frame_Context_Type'(Context);
      Top_Context.Parent_Context := Parent;
   end Push_Frame_Context;

   -----------------------
   -- Pop_Frame_Context --
   -----------------------

   procedure Pop_Frame_Context is
   begin
      Top_Frame.Top_Context := Top_Context.Parent_Context;
   end Pop_Frame_Context;

   -------------------------------
   -- Push_Match_Groups_Section --
   -------------------------------

   procedure Push_Match_Groups_Section is
   begin
      Top_Frame.Group_Sections.Append (new Matched_Groups_Type);
   end Push_Match_Groups_Section;

   ------------------------------
   -- Pop_Match_Groups_Section --
   ------------------------------

   procedure Pop_Match_Groups_Section is
   begin
      Top_Frame.Group_Sections.Delete_Last;
   end Pop_Match_Groups_Section;

   ----------------------------------
   -- Update_Top_And_Parent_Frames --
   ----------------------------------

   procedure Update_Top_And_Parent_Frames is
   begin
      if Data_Frame_Stack.Length > 0 then
         Top_Frame_Ref := Data_Frame_Stack.Last_Element;
      else
         Top_Frame_Ref := null;
      end if;

      if Data_Frame_Stack.Length > 1 then
         Parent_Frame_Ref :=
           Data_Frame_Stack.Element (Data_Frame_Stack.Last_Index - 1);
      else
         Parent_Frame_Ref := null;
      end if;

      Update_Top_Object;
   end Update_Top_And_Parent_Frames;

   ----------------
   -- Push_Frame --
   ----------------

   procedure Push_Frame (Lexical_Scope : access T_Entity_Type'Class) is
      New_Frame : constant Data_Frame := new Data_Frame_Type;
   begin
      New_Frame.Lexical_Scope := T_Entity (Lexical_Scope);
      New_Frame.Top_Context   := new Frame_Context_Type;

      if Parent_Frame /= null then
         --  If there's a parent frame, some context data needs to be passed
         --  to the child:

         --  the link to the visit decision variable, as in:
         --     wrap A_Template ()
         --  the frame within A_Template can decide to change the iteration
         --  that leads to the wrap command
         New_Frame.Top_Context.Visit_Decision := Top_Context.Visit_Decision;

         --  The indentation, as indentation of a child frame starts at the
         --  identation level of its parent (so that so functions and template
         --  can create their own indented section contributing to the global
         --  output).
         New_Frame.Top_Context.Indent := Top_Context.Indent;
      end if;

      --  Each new frame create a new temporary names registry
      New_Frame.Temp_Names := new Tmp_Maps.Map;

      Data_Frame_Stack.Append (New_Frame);
      Update_Top_And_Parent_Frames;
   end Push_Frame;

   ----------------
   -- Push_Frame --
   ----------------

   procedure Push_Frame (Frame : Data_Frame) is
   begin
      Data_Frame_Stack.Append (Frame);
      Update_Top_And_Parent_Frames;
   end Push_Frame;

   ----------------
   -- Push_Frame --
   ----------------

   procedure Push_Frame (A_Closure : Closure) is
   begin
      --  First, push a frame on the captured lexical scope
      Push_Frame (A_Closure.Lexical_Scope);

      --  Copy symbols from the closure to the new symbol tree
      Top_Frame.Symbols := A_Closure.Captured_Symbols.Copy;

      --  All closure share the same temporary names
      Top_Frame.Temp_Names := A_Closure.Temp_Names;

      --  Retreives the left value at the point of capture
      Top_Context.Left_Value := A_Closure.Left_Value;

      --  Retreives the implicit it value at the point of capture
      Push_Implicit_It (A_Closure.Implicit_It);
   end Push_Frame;

   ---------------
   -- Pop_Frame --
   ---------------

   procedure Pop_Frame is
   begin
      Data_Frame_Stack.Delete_Last;
      Update_Top_And_Parent_Frames;
   end Pop_Frame;

   ---------------------
   -- Get_Implicit_It --
   ---------------------

   function Get_Implicit_It return W_Object is
   begin
      return Top_Context.It_Value;
   end Get_Implicit_It;

end Wrapping.Runtime.Frames;
