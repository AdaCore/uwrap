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

with Wrapping.Utils;            use Wrapping.Utils;
with Wrapping.Runtime.Commands; use Wrapping.Runtime.Commands;
with Wrapping.Runtime.Objects;  use Wrapping.Runtime.Objects;

package body Wrapping.Runtime.Frames is

   Data_Frame_Stack : Data_Frame_Vectors.Vector;

   procedure Update_Object;

   procedure Update_Frames;

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
      use Semantic.Structure;

      Scope : T_Entity := A_Frame.Lexical_Scope;
   begin
      while Scope /= null and then Scope.all not in T_Module_Type'Class loop
         Scope := Scope.Parent;
      end loop;

      return T_Module (Scope);
   end Get_Module;

   -------------------
   -- Update_Object --
   -------------------

   procedure Update_Object is
   begin
      if Top_Frame.Data_Stack.Length > 0 then
         Top_Object_Ref := Top_Frame.Data_Stack.Last_Element;
      end if;
   end Update_Object;

   -----------------
   -- Push_Object --
   -----------------

   procedure Push_Object (Object : access W_Object_Type'Class) is
   begin
      pragma Assert (if Object.all in W_Reference_Type'Class then
                        W_Reference (Object).Value /= null);

      Top_Frame.Data_Stack.Append (W_Object (Object));
      Update_Object;
   end Push_Object;

   ----------------------
   -- Push_Implicit_It --
   ----------------------

   procedure Push_Implicit_It (Object : access W_Object_Type'Class) is
   begin
      Push_Object
        (W_Object'
           (new W_Reference_Type'
              (Value  => W_Object (Object), Is_Implicit_It => True,
               others => <>)));
   end Push_Implicit_It;

   ---------------------------
   -- Push_Allocated_Entity --
   ---------------------------

   procedure Push_Allocated_Entity (Object : access W_Object_Type'Class) is
   begin
      Push_Object
        (W_Object'
           (new W_Reference_Type'
              (Value  => W_Object (Object), Is_Allocated => True,
               others => <>)));
   end Push_Allocated_Entity;

   -------------------------
   -- Push_Temporary_Name --
   -------------------------

   procedure Push_Temporary_Name (Name : Text_Type; Counter : in out Integer)
   is
   begin
      if Top_Frame.Temp_Names.Contains (Name) then
         Push_Object (To_W_String (Top_Frame.Temp_Names.Element (Name)));
      else
         Counter := Counter + 1;

         declare
            Tmp : Text_Type :=
              "Temp_" & (if Name /= "" then Name & "_" else "") &
              Trim (Integer'Wide_Wide_Image (Counter), Both);
         begin
            Top_Frame.Temp_Names.Insert (Name, Tmp);

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
      Update_Object;
   end Pop_Object;

   ------------------------
   -- Pop_Underneath_Top --
   ------------------------

   procedure Pop_Underneath_Top is
   begin
      Top_Frame.Data_Stack.Delete (Integer (Top_Frame.Data_Stack.Length) - 1);
      Update_Object;
   end Pop_Underneath_Top;

   ----------------
   -- Pop_Object --
   ----------------

   function Pop_Object return W_Object is
      Result : W_Object;
   begin
      Result := Top_Frame.Data_Stack.Last_Element;
      Pop_Object;
      Update_Object;

      return Result;
   end Pop_Object;

   ---------------------
   -- Top_Is_Implicit --
   ---------------------

   function Top_Is_Implicit return Boolean is
      Top : W_Object := Top_Object;
   begin
      return
        Top.all in W_Reference_Type'Class
        and then W_Reference (Top).Is_Implicit;
   end Top_Is_Implicit;

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
      Push_Frame_Context_No_Match;
      Top_Context.Is_Root_Selection := True;
   end Push_Frame_Context_Parameter;

   ---------------------------------------------
   -- Push_Frame_Context_Parameter_With_Match --
   ---------------------------------------------

   procedure Push_Frame_Context_Parameter_With_Match (Object : W_Object) is
   begin
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
      Push_Frame_Context;
      Top_Context.Function_Result_Callback := null;
      Top_Context.Outer_Expr_Action        := Action_None;
   end Push_Frame_Context_No_Pick;

   ------------------------
   -- Push_Frame_Context --
   ------------------------

   procedure Push_Frame_Context (Context : Frame_Context_Type) is
      Parent : Frame_Context := Top_Context;
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

   -------------------
   -- Update_Frames --
   -------------------

   procedure Update_Frames is
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
   end Update_Frames;

   ----------------
   -- Push_Frame --
   ----------------

   procedure Push_Frame (Lexical_Scope : access T_Entity_Type'Class) is
      New_Frame : Data_Frame := new Data_Frame_Type;
   begin
      New_Frame.Lexical_Scope := T_Entity (Lexical_Scope);
      New_Frame.Top_Context   := new Frame_Context_Type;

      if Parent_Frame /= null then
         New_Frame.Top_Context.Allocate_Callback
           := Top_Context.Allocate_Callback;
         New_Frame.Top_Context.Visit_Decision := Top_Context.Visit_Decision;
         New_Frame.Top_Context.Yield_Callback := Top_Context.Yield_Callback;
         New_Frame.Top_Context.Indent := Top_Context.Indent;
      end if;

      New_Frame.Temp_Names := new Text_Maps.Map;

      Data_Frame_Stack.Append (New_Frame);
      Update_Frames;
   end Push_Frame;

   ----------------
   -- Push_Frame --
   ----------------

   procedure Push_Frame (Frame : Data_Frame) is
   begin
      Data_Frame_Stack.Append (Frame);
      Update_Frames;
   end Push_Frame;

   ----------------
   -- Push_Frame --
   ----------------

   procedure Push_Frame (A_Closure : Closure) is
      Copy_Symbols : W_Object_Maps.Map;
   begin
      Push_Frame (A_Closure.Lexical_Scope);

      Copy_Symbols := A_Closure.Captured_Symbols.Copy;
      Top_Frame.Symbols.Move (Copy_Symbols);
      Top_Frame.Temp_Names := A_Closure.Temp_Names;
      Top_Context.Left_Value := A_Closure.Left_Value;

      if A_Closure.Implicit_It /= null then
         Push_Implicit_It (A_Closure.Implicit_It);
      end if;
   end Push_Frame;

   ---------------
   -- Pop_Frame --
   ---------------

   procedure Pop_Frame is
   begin
      Data_Frame_Stack.Delete_Last;
      Update_Frames;
   end Pop_Frame;

   ---------------------
   -- Get_Implicit_It --
   ---------------------

   function Get_Implicit_It return W_Object is
   begin
      for I in reverse
        Top_Frame.Data_Stack.First_Index .. Top_Frame.Data_Stack.Last_Index
      loop
         if Top_Frame.Data_Stack.Element (I).all in W_Reference_Type'Class
           and then
             W_Reference (Top_Frame.Data_Stack.Element (I)).Is_Implicit_It
         then
            return W_Reference (Top_Frame.Data_Stack.Element (I)).Value;
         end if;
      end loop;

      return null;
   end Get_Implicit_It;

end Wrapping.Runtime.Frames;
