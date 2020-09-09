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
-- General Public License  distributed with UWrap; see file COPYING3.  If --
-- not, go to http://www.gnu.org/licenses for a complete copy of the        --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO;           use Ada.Wide_Wide_Text_IO;
with Ada.Containers;                  use Ada.Containers;
with Ada.Characters.Conversions;      use Ada.Characters.Conversions;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Fixed;     use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings;                     use Ada.Strings;
with Ada.Tags;                        use Ada.Tags;
with GNAT.Regpat;                     use GNAT.Regpat;

with Langkit_Support.Diagnostics;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libtemplatelang.Common; use Libtemplatelang.Common;

with Wrapping.Regex;              use Wrapping.Regex;
with Wrapping.Semantic.Analysis;  use Wrapping.Semantic.Analysis;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Utils;              use Wrapping.Utils;
with Wrapping.Runtime.Functions;  use Wrapping.Runtime.Functions;

package body Wrapping.Runtime.Analysis is

   procedure Handle_Identifier (Node : Template_Node'Class);
   procedure Handle_Call (Expr : T_Expr) with
      Post => Top_Frame.Data_Stack.Length =
      Top_Frame.Data_Stack.Length'Old + 1;

   pragma Warnings (Off, "postcondition does not mention");
   function Handle_Template_Call
     (A_Template_Instance : W_Template_Instance; Args : T_Arg_Vectors.Vector)
      return Visit_Action with
      Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;
   pragma Warnings (Off, "postcondition does not mention");

   procedure Handle_Fold
     (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) with
      Post => Top_Frame.Data_Stack.Length =
      Top_Frame.Data_Stack.Length'Old + 1;

   procedure Handle_Filter
     (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) with
      Post => Top_Frame.Data_Stack.Length =
      Top_Frame.Data_Stack.Length'Old + 1;

   procedure Handle_New (Create_Tree : T_Create_Tree) with
      Post => Top_Frame.Data_Stack.Length =
      Top_Frame.Data_Stack.Length'Old + 1;
   procedure Handle_All
     (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) with
      Post => Top_Frame.Data_Stack.Length =
      Top_Frame.Data_Stack.Length'Old + 1;
   procedure Handle_Selector
     (Expr : T_Expr; Suffix : in out T_Expr_Vectors.Vector) with
      Post => Top_Frame.Data_Stack.Length =
      Top_Frame.Data_Stack.Length'Old + 1;

   procedure Handle_Arithmetic_Operator (Expr : T_Expr);

   procedure Evaluate_String
     (Expr : T_Expr;
      On_Group : access procedure (Index : Integer; Value : W_Object) := null;
      On_Expression : access procedure (Expr : T_Expr) := null) with
      Post => Top_Frame.Data_Stack.Length =
      Top_Frame.Data_Stack.Length'Old + 1;

   procedure Apply_Wrapping_Program
     (It : W_Node; Lexical_Scope : access T_Entity_Type'Class) with
      Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;

      --------------------------
      -- Call_Convert_To_Text --
      --------------------------

   procedure Call_Convert_To_Text
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
   begin
      if Params.Length in 1 .. 2 then
         Push_Frame_Context_Parameter;

         Push_Object
           (W_Object'
              (new W_Text_Conversion_Type'
                 (An_Object =>
                    Evaluate_Expression (Params.Element (1).Expr))));

         Pop_Frame_Context;

         if Params.Length = 2 then
            Push_Match_Result (Top_Object, Params.Element (2).Expr);
            Delete_Object_At_Position (-2);
         end if;
      else
         Error ("conversion takes up to 2 arguments");
      end if;
   end Call_Convert_To_Text;

   ----------------------------
   -- Call_Convert_To_String --
   ----------------------------

   procedure Call_Convert_To_String
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Slice : Buffer_Slice;
   begin
      if Params.Length in 1 .. 2 then
         Push_Frame_Context_Parameter;
         Push_Buffer_Cursor;
         Slice :=
           Evaluate_Expression
             (Params.Element (1).Expr).Write_String;
         Pop_Buffer_Cursor;

         Push_Object
           (To_W_String
              (Buffer.Str
                   (Slice.First.Offset .. Slice.Last.Offset)));

         if Params.Length = 2 then
            Push_Match_Result (Top_Object, Params.Element (2).Expr);
            Delete_Object_At_Position (-2);
         end if;
      else
         Error ("conversion takes up to 2 arguments");
      end if;
   end Call_Convert_To_String;

   -------------------------
   -- Push_Error_Location --
   -------------------------

   procedure Push_Error_Location (An_Entity : access T_Entity_Type'Class) is
   begin
      Push_Error_Location
        (An_Entity.Unit.Get_Filename, Start_Sloc (An_Entity.Sloc));
   end Push_Error_Location;

   ------------------------
   -- Pop_Error_Location --
   ------------------------

   procedure Pop_Error_Location is
   begin
      Wrapping.Pop_Error_Location;
   end Pop_Error_Location;

   -------------------
   -- Update_Object --
   -------------------

   procedure Update_Object is
   begin
      if Top_Frame.Data_Stack.Length > 0 then
         Top_Object := Top_Frame.Data_Stack.Last_Element;
      end if;
   end Update_Object;

   -----------------
   -- Push_Object --
   -----------------

   procedure Push_Object (Object : access W_Object_Type'Class) is
   begin
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

   -----------------------------
   -- Push_Intrinsic_Function --
   -----------------------------

   procedure Push_Intrinsic_Function (Prefix : W_Object; A_Call : Call_Access)
   is
   begin
      Push_Object
        (W_Object'
           (new W_Intrinsic_Function_Type'
              (Prefix => Prefix, Call => A_Call, others => <>)));
   end Push_Intrinsic_Function;

   ----------------
   -- Pop_Object --
   ----------------

   procedure Pop_Object (Number : Positive := 1) is
   begin
      Top_Frame.Data_Stack.Delete_Last (Count_Type (Number));
      Update_Object;
   end Pop_Object;

   -------------------------------
   -- Delete_Object_At_Position --
   -------------------------------

   procedure Delete_Object_At_Position (Position : Integer) is
   begin
      if Position > 0 then
         Top_Frame.Data_Stack.Delete (Position);
      else
         Top_Frame.Data_Stack.Delete
           (Integer (Top_Frame.Data_Stack.Length) + Position + 1);
      end if;
      Update_Object;
   end Delete_Object_At_Position;

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
      Push_Frame_Context (Top_Frame.Top_Context.all);
   end Push_Frame_Context;

   ----------------------------------
   -- Push_Frame_Context_Parameter --
   ----------------------------------

   procedure Push_Frame_Context_Parameter is
   begin
      Push_Frame_Context_No_Match;
      Top_Frame.Top_Context.Is_Root_Selection := True;
   end Push_Frame_Context_Parameter;

   ---------------------------------------------
   -- Push_Frame_Context_Parameter_With_Match --
   ---------------------------------------------

   procedure Push_Frame_Context_Parameter_With_Match (Object : W_Object) is
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Is_Root_Selection   := True;
      Top_Frame.Top_Context.Match_Mode          := Match_Ref_Default;
      Top_Frame.Top_Context.Outer_Expr_Callback :=
        Outer_Expression_Match'Access;
      Top_Frame.Top_Context.Outer_Object := Object;
   end Push_Frame_Context_Parameter_With_Match;

   ---------------------------------
   -- Push_Frame_Context_No_Outer --
   ---------------------------------

   procedure Push_Frame_Context_No_Outer is
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Pick_Callback       := null;
      Top_Frame.Top_Context.Outer_Expr_Callback := null;
      Top_Frame.Top_Context.Outer_Object        := null;
   end Push_Frame_Context_No_Outer;

   ---------------------------------
   -- Push_Frame_Context_No_Match --
   ---------------------------------

   procedure Push_Frame_Context_No_Match is
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode          := Match_None;
      Top_Frame.Top_Context.Outer_Expr_Callback := null;
      Top_Frame.Top_Context.Outer_Object        := null;
   end Push_Frame_Context_No_Match;

   --------------------------------
   -- Push_Frame_Context_No_Pick --
   --------------------------------

   procedure Push_Frame_Context_No_Pick is
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Pick_Callback       := null;
      Top_Frame.Top_Context.Outer_Expr_Callback := null;
   end Push_Frame_Context_No_Pick;

   ------------------------
   -- Push_Frame_Context --
   ------------------------

   procedure Push_Frame_Context (Context : Frame_Context_Type) is
      Parent : Frame_Context := Top_Frame.Top_Context;
   begin
      Top_Frame.Top_Context := new Frame_Context_Type'(Context);
      Top_Frame.Top_Context.Parent_Context := Parent;
   end Push_Frame_Context;

   -----------------------
   -- Pop_Frame_Context --
   -----------------------

   procedure Pop_Frame_Context is
   begin
      Top_Frame.Top_Context := Top_Frame.Top_Context.Parent_Context;
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
         Top_Frame := Data_Frame_Stack.Last_Element;
      else
         Top_Frame := null;
      end if;

      if Data_Frame_Stack.Length > 1 then
         Parent_Frame :=
           Data_Frame_Stack.Element (Data_Frame_Stack.Last_Index - 1);
      else
         Parent_Frame := null;
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
         New_Frame.Top_Context.Allocate_Callback :=
           Top_Frame.Top_Context.Allocate_Callback;
         New_Frame.Top_Context.Visit_Decision :=
           Top_Frame.Top_Context.Visit_Decision;
         New_Frame.Top_Context.Yield_Callback :=
           Top_Frame.Top_Context.Yield_Callback;
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

   function Get_Implicit_It (From : Data_Frame := Top_Frame) return W_Object is
   begin
      for I in reverse
        From.Data_Stack.First_Index .. From.Data_Stack.Last_Index
      loop
         if From.Data_Stack.Element (I).all in W_Reference_Type'Class
           and then W_Reference (From.Data_Stack.Element (I)).Is_Implicit_It
         then
            return W_Reference (From.Data_Stack.Element (I)).Value;
         end if;
      end loop;

      return null;
   end Get_Implicit_It;

   -----------
   -- Match --
   -----------

   function Match (Pattern, Text : Text_Type) return Boolean is
      Text_Str : String    := To_String (Text);
      Matches  : Match_Obj := Match (Compile (To_String (Pattern)), Text_Str);
   begin
      if Wrapping.Regex.No_Match (Matches) then
         return False;
      end if;

      for I in 1 .. Matches.Matches.Element'Last loop
         declare
            Matched_Text : Text_Type :=
              To_Text
                (Text_Str
                   (Matches.Matches.Element (I).First ..
                        Matches.Matches.Element (I).Last));
            Name : Text_Type := To_Text (Get_Capture_Name (Matches, I));
         begin
            Top_Frame.Group_Sections.Last_Element.Groups.Append
              (W_Object (To_W_String (Matched_Text)));

            if Name /= "" then
               Include_Symbol (Name, W_Object (To_W_String (Matched_Text)));
            end if;
         end;
      end loop;

      return True;
   end Match;

   ---------------------------
   -- Apply_Template_Action --
   ---------------------------

   procedure Apply_Template_Action
     (It : W_Node; Template_Clause : T_Template_Section)
   is
      A_Template_Instance : W_Template_Instance;
      Self_Weave          : Boolean := False;
      Result              : W_Object;

      --  TODO: is this still necessary?
      Dummy_Action : Visit_Action;
   begin
      Push_Error_Location (Template_Clause.Node);
      Push_Implicit_It (It);

      if Template_Clause.Call.Is_Null then
         --  We've set a null template - the objective is to prevent this
         --  entity to be wrapped by this template.

         if Template_Clause.Call.Args.Length /= 1 then
            Error ("expected one argument to null");
         end if;

         Push_Frame_Context;

         --  There's nothing to check on the expression below. Deactivate the
         --  expression callback (otherwise, it may perform wrong calls, either
         --  to an unwanted check, or to the outer pick function.
         Top_Frame.Top_Context.Outer_Expr_Callback := null;

         Evaluate_Expression (Template_Clause.Call.Args.Element (1).Expr);
         Result := Pop_Object.Dereference;
         Pop_Frame_Context;

         if Result.all not in W_Static_Entity_Type'Class
           or else W_Static_Entity (Result).An_Entity.all not in
             T_Template_Type'Class
         then
            Error ("expected template reference");
         end if;

         It.Forbidden_Template_Names.Include
           (T_Template (W_Static_Entity (Result).An_Entity).Full_Name);

         --  TODO: remove the template if it's already been created in the
         --  context of a weave clause
      else
         if Template_Clause.Call.Reference = null then
            --  No name to the call, that means that we're expecting to
            --  self-weave the current template.
            if Template_Clause.Kind /= Weave_Kind then
               Error
                 ("self wrap not allowed, either weave or"
                  & " provide a template or visitor name");
            elsif It.all in W_Template_Instance_Type'Class then
               A_Template_Instance := W_Template_Instance (It);
            else
               Error ("only template instances can be self weaved");
            end if;

            Self_Weave := True;
         elsif Template_Clause.Call.Reference.all in T_Template_Type'Class then
            A_Template_Instance :=
              It.Get_Template_Instance
                (T_Template (Template_Clause.Call.Reference));

            if Template_Clause.Kind = Walk_Kind then
               A_Template_Instance :=
                 It.Create_Template_Instance
                   (T_Template (Template_Clause.Call.Reference), False);
            elsif
              (Template_Clause.Kind = Weave_Kind
               or else A_Template_Instance = null
               or else A_Template_Instance.Is_Wrapping = False)
              and then not It.Forbidden_Template_Names.Contains
                (Template_Clause.Call.Reference.Full_Name)
            then
               if A_Template_Instance = null then
                  A_Template_Instance :=
                    It.Create_Template_Instance
                      (T_Template (Template_Clause.Call.Reference), True);
               end if;

               if Template_Clause.Kind = Wrap_Kind then
                  A_Template_Instance.Is_Wrapping := True;
               end if;
            else
               A_Template_Instance := null;
            end if;
         else
            Error ("unexpected template call reference type");
         end if;

         if A_Template_Instance /= null then
            if Template_Clause.Call.Captured_Name /= "" then
               Include_Symbol
                 (To_Text (Template_Clause.Call.Captured_Name),
                  W_Object (A_Template_Instance));
            end if;

            Dummy_Action :=
              Handle_Template_Call
                (A_Template_Instance, Template_Clause.Call.Args);
         end if;
      end if;

      Pop_Object;
      Pop_Error_Location;
   end Apply_Template_Action;

   procedure Handle_Command_Front (Command : T_Command) with
      Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;

   procedure Handle_Command_Front_Nodefer (Command : T_Command) with
      Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;
      --  Same as Handle_Command_Front but skips the defer part of the command,
      --  computing its actions right away. This assumes that the command
      --  context has been previously installed and will be cleared afterwards
      --  (see Install_Command_Context / Uninstall_Command_Context).

   procedure Handle_Command_Back (Command : T_Command) with
      Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;

      --------------------
      -- Handle_Command --
      --------------------

   procedure Handle_Command (Command : T_Command; It : W_Node) is
   begin
      --  The command is the enclosing scope for all of its clauses. It will in
      --  particular receive the matching groups and the temporary values that
      --  can be used consistently in the various clauses
      Push_Frame (Command);
      Push_Implicit_It (It);

      Handle_Command_Front (Command);

      Pop_Object; -- Pop It.
      Pop_Frame;
   end Handle_Command;

   -------------------------------
   -- Evaluate_Match_Expression --
   -------------------------------

   function Evaluate_Match_Expression (Expr : T_Expr) return Boolean is
   begin
      if Expr /= null then
         Push_Frame_Context;
         Top_Frame.Top_Context.Match_Mode := Match_Ref_Default;
         Evaluate_Expression (Expr);
         Pop_Frame_Context;

         return Pop_Object /= Match_False;
      else
         return True;
      end if;
   end Evaluate_Match_Expression;

   -----------------------
   -- Allocate_Detached --
   -----------------------

   procedure Allocate_Detached (E : access W_Object_Type'Class) is
   begin
      --  when allocating an object outside of a browsing function, nothign
      --  special to do
      null;
   end Allocate_Detached;

   -----------------------------
   -- Install_Command_Context --
   -----------------------------

   procedure Install_Command_Context (Command : T_Command) is
   begin
      Top_Frame.Top_Context.Allocate_Callback :=
        Allocate_Detached'Unrestricted_Access;
      Top_Frame.Top_Context.Outer_Expr_Callback :=
        Outer_Expression_Match'Access;
      Top_Frame.Top_Context.Current_Command   := Command;
      Top_Frame.Top_Context.Is_Root_Selection := True;
      Top_Frame.Top_Context.Outer_Object      := Top_Object;

      Push_Match_Groups_Section;
   end Install_Command_Context;

   -------------------------------
   -- Uninstall_Command_Context --
   -------------------------------

   procedure Uninstall_Command_Context is
   begin
      Pop_Match_Groups_Section;
   end Uninstall_Command_Context;

   --------------------------
   -- Handle_Command_Front --
   --------------------------

   procedure Handle_Command_Front (Command : T_Command) is
   begin
      if Top_Frame.Interrupt_Program then
         return;
      end if;

      if Command.Defer then
         declare
            New_Command : Deferred_Command := new Deferred_Command_Type;
         begin
            New_Command.Command   := Command;
            New_Command.A_Closure :=
              Capture_Closure (Command.Deferred_Closure);

            Deferred_Commands.Append (New_Command);
         end;
      else
         Install_Command_Context (Command);
         Handle_Command_Front_Nodefer (Command);
         Uninstall_Command_Context;
      end if;
   end Handle_Command_Front;

   ----------------------------------
   -- Handle_Command_Front_Nodefer --
   ----------------------------------

   procedure Handle_Command_Front_Nodefer (Command : T_Command) is
   begin
      if Evaluate_Match_Expression (Command.Match_Expression) then
         if Command.Pick_Expression /= null then
            --  When evaluating a pick expression, the wrapping program will
            --  be evaluated by the outer epxression callback. This caters
            --  in particular for cases where more than one object is being
            --  retreived.

            Top_Frame.Top_Context.Outer_Expr_Callback :=
              Outer_Expression_Pick'Access;

            Evaluate_Expression (Command.Pick_Expression);
            Pop_Object;

            Top_Frame.Top_Context.Outer_Expr_Callback :=
              Outer_Expression_Match'Access;
         else
            Handle_Command_Back (Command);
         end if;
      else
         if Command.Command_Sequence /= null then
            declare
               Else_Section : T_Command_Sequence_Element :=
                 Command.Command_Sequence.First_Element;
            begin
               while Else_Section /= null loop
                  while Else_Section /= null and then not Else_Section.Is_Else
                  loop
                     Else_Section := Else_Section.Next_Element;
                  end loop;

                  exit when Else_Section = null;

                  if Evaluate_Match_Expression (Else_Section.Match_Expression)
                  then
                     Handle_Command_Sequence (Else_Section);
                     exit;
                  end if;

                  Else_Section := Else_Section.Next_Element;
               end loop;
            end;
         end if;
      end if;
   end Handle_Command_Front_Nodefer;

   -------------------------
   -- Handle_Command_Back --
   -------------------------

   procedure Handle_Command_Back (Command : T_Command) is
      Top : W_Object := Top_Object.Dereference;
      It  : W_Node;
   begin
      if Top_Frame.Interrupt_Program then
         return;
      end if;

      if Top = Match_False then
         return;
      elsif Top.all in W_Node_Type'Class then
         It := W_Node (Top);
      elsif Top.all in W_Static_Entity_Type then
         It :=
           W_Node (Get_Object_For_Entity (W_Static_Entity (Top).An_Entity));
      else
         Error ("can't pick selected object");
      end if;

      Push_Implicit_It (It);

      if Command.Command_Sequence /= null then
         Handle_Command_Sequence (Command.Command_Sequence.First_Element);
      elsif Command.Template_Section /= null then
         if Command.Template_Section.A_Visit_Action /= Unknown then
            --  TODO: consider differences between weave and wrap here
            --  TODO: This doesn't consider different visits, each should have
            --  its own decision

            if Top_Frame.Top_Context.Visit_Decision.all = Unknown then
               Top_Frame.Top_Context.Visit_Decision.all :=
                 Command.Template_Section.A_Visit_Action;
            end if;
         elsif Command.Template_Section.Call.Reference /= null
           or else Command.Template_Section.Call.Args.Length /= 0
         then
            --  There is an explicit template call. Pass this on either the
            --  current template or the whole tree

            Apply_Template_Action (It, Command.Template_Section);
         end if;
      end if;

      Pop_Object;
   end Handle_Command_Back;

   ----------------------------
   -- Handle_Defered_Command --
   ----------------------------

   function Handle_Defered_Command (Command : Deferred_Command) return Boolean
   is
      Result : Boolean := False;
   begin
      Push_Frame (Command.A_Closure);

      Install_Command_Context (Command.Command);

      --  Only commands with a command sequence and no else part can be defered

      if Evaluate_Match_Expression (Command.Command.Defer_Expression) then
         Handle_Command_Front_Nodefer (Command.Command);
         Result := True;
      end if;

      Uninstall_Command_Context;

      Pop_Frame;

      return Result;
   end Handle_Defered_Command;

   -----------------------------
   -- Handle_Command_Sequence --
   -----------------------------

   procedure Handle_Command_Sequence (Sequence : T_Command_Sequence_Element) is
      Seq           : T_Command_Sequence_Element := Sequence;
      Calling_Frame : Data_Frame                 := Parent_Frame;
      Called_Frame  : Data_Frame                 := Top_Frame;
      New_Ref       : W_Reference;
   begin
      while Seq /= null loop
         --  First, create variables for this scope. This need to be done
         --  before any expression evaluation, as initialization and parameter
         --  expressions may reference other variables declared before. Also
         --  see Handle_Template_Call for details on the evaluation of template
         --  parameters.

         for A_Var of Seq.Vars loop
            declare
               Name : Text_Type := A_Var.Name_Node.Text;
            begin
               New_Ref := new W_Reference_Type;

               case A_Var.Kind is
                  when Text_Kind =>
                     --  Symbols contained in templates are references to
                     --  values. Create the reference and the referenced
                     --  empty value here.

                     New_Ref.Value := new W_Text_Vector_Type;

                  when Set_Kind =>
                     New_Ref.Value := new W_Set_Type;

                  when Map_Kind =>
                     New_Ref.Value := new W_Map_Type;

                  when Vector_Kind =>
                     New_Ref.Value := new W_Vector_Type;

                  when Object_Kind =>
                     New_Ref.Value := Match_False;

                  when others =>
                     Error ("variable kind not supported for templates");

               end case;

               Include_Symbol (Name, W_Object (New_Ref));

               if Top_Frame.Current_Template /= null then
                  W_Template_Instance (Top_Frame.Current_Template)
                    .Indexed_Variables
                    .Insert
                    (Name, New_Ref);
                  W_Template_Instance (Top_Frame.Current_Template)
                    .Ordered_Variables
                    .Append
                    (New_Ref);
               end if;
            end;
         end loop;

         --  Second, call initializations

         for A_Var of Seq.Vars loop
            declare
               Name : Text_Type := A_Var.Name_Node.Text;
            begin
               New_Ref := W_Reference (Top_Frame.Symbols.Element (Name));

               if A_Var.Init_Expr /= null then
                  Push_Frame_Context_No_Match;
                  Top_Frame.Top_Context.Left_Value        := New_Ref.Value;
                  Top_Frame.Top_Context.Is_Root_Selection := True;

                  Evaluate_Expression (A_Var.Init_Expr);

                  Pop_Frame_Context;

                  New_Ref.Value := Pop_Object;
               end if;
            end;
         end loop;

         --  Third, call parameter expressions. This needs to be done in the
         --  context of the calling frame, which is temporarily restored for
         --  this purpose. Calling frame may be null here, e.g. in the case
         --  of a defered command. In this case, there's no way to modify the
         --  value of the variable.

         if Calling_Frame /= null then
            Push_Frame (Calling_Frame);

            for A_Var of Seq.Vars loop
               declare
                  Name : Text_Type := A_Var.Name_Node.Text;
               begin
                  New_Ref := W_Reference (Called_Frame.Symbols.Element (Name));

                  if Calling_Frame.Template_Parameters_Position.Length > 0
                    or else Calling_Frame.Template_Parameters_Names.Contains
                      (Name)
                  then
                     Push_Frame_Context_No_Match;
                     Top_Frame.Top_Context.Left_Value        := New_Ref.Value;
                     Top_Frame.Top_Context.Is_Root_Selection := True;

                     if Calling_Frame.Template_Parameters_Position.Length > 0
                     then
                        Evaluate_Expression
                          (Calling_Frame.Template_Parameters_Position
                             .First_Element);
                        Calling_Frame.Template_Parameters_Position
                          .Delete_First;
                     else
                        Evaluate_Expression
                          (Calling_Frame.Template_Parameters_Names.Element
                             (Name));
                        Calling_Frame.Template_Parameters_Names.Delete (Name);
                     end if;

                     New_Ref.Value := Pop_Object;

                     Pop_Frame_Context;
                  end if;
               end;
            end loop;

            Pop_Frame;
         end if;

         --  Then execute command in reverse

         for C of reverse Seq.Commands loop
            exit when Top_Frame.Interrupt_Program;

            Handle_Command_Front (T_Command (C));
         end loop;

         exit when Top_Frame.Interrupt_Program;

         Seq := Seq.Next_Element;

         --  Run all command sequences up until hitting an else section. If
         --  such section is hit, it's controlled by a top level match section,
         --  which already validated the current sequence as matching. The else
         --  wasn't picked. Stop runnint here.

         exit when Seq = null or else Seq.Is_Else;
      end loop;
   end Handle_Command_Sequence;

   ----------------------------
   -- Apply_Wrapping_Program --
   ----------------------------

   procedure Apply_Wrapping_Program
     (It : W_Node; Lexical_Scope : access T_Entity_Type'Class)
   is
   begin
      Push_Frame (Lexical_Scope);

      for Wrapping_Entity of reverse Lexical_Scope.Children_Ordered loop
         if Wrapping_Entity.all in T_Command_Type then
            Handle_Command (T_Command (Wrapping_Entity), It);
         elsif Wrapping_Entity.all in T_Module_Type'Class
           or else Wrapping_Entity.all in T_Namespace_Type'Class
         then
            Apply_Wrapping_Program (It, Wrapping_Entity);
         end if;
      end loop;

      Pop_Frame;
   end Apply_Wrapping_Program;

   ---------------------
   -- Analyze_Visitor --
   ---------------------

   function Analyze_Visitor
     (E : access W_Object_Type'Class; Result : out W_Object)
      return Visit_Action
   is
      A_Visit_Action : Visit_Action         := Into;
      N              : W_Node;
      Visit_Result   : aliased Visit_Action := Unknown;
   begin
      Push_Frame (Wrapping.Semantic.Analysis.Root);
      Top_Frame.Top_Context.Visit_Decision := Visit_Result'Unchecked_Access;

      Result := null;

      --  Check if this entity has already been analyzed through this visitor
      --  invocation.

      --  First, pop any id in the entity that may be greater than the current
      --  one. If they are greater and we're on a lower one, it means that they
      --  are over.

      if E.all not in W_Node_Type'Class then
         Error ("expected node type");
      end if;

      N := W_Node (E);

      while N.Visited_Stack.Length > 0
        and then N.Visited_Stack.Last_Element > Current_Visitor_Id
      loop
         N.Visited_Stack.Delete_Last;
      end loop;

      if N.Visited_Stack.Length > 0
        and then N.Visited_Stack.Last_Element = Current_Visitor_Id
      then
         --  If the last id in the stack is the current one, they we've already
         --  visited this entity. We already also made the decisions on sub
         --  entities. Stop the iteration.

         Pop_Frame;
         return Over;
      else
         --  Otherwise, stack this visitor Id and visit. We need not to remove
         --  this id at the end, as to make sure that potential iterations on
         --  this visit don't cover this node again

         N.Visited_Stack.Append (Current_Visitor_Id);

         Apply_Wrapping_Program (N, Wrapping.Semantic.Analysis.Root);

         Pop_Frame;

         if Visit_Result = Unknown then
            return Into;
         else
            return Visit_Result;
         end if;
      end if;
   end Analyze_Visitor;

   -------------------
   -- Analyse_Input --
   -------------------

   procedure Analyse_Input (Root_Entity : W_Node) is
      Dummy_Action    : Visit_Action;
      Traverse_Result : W_Object;
   begin
      --  Set the visitor id - we're on the main iteration, id is 0.

      Current_Visitor_Id := 0;

      Push_Frame (Wrapping.Semantic.Analysis.Root);
      Dummy_Action :=
        Root_Entity.Traverse
          (Child_Depth, True, Traverse_Result, Analyze_Visitor'Access);
      Pop_Frame;
   end Analyse_Input;

   -----------------------
   -- Analyze_Templates --
   -----------------------

   procedure Analyze_Templates is
      A_Template_Instance : W_Template_Instance;
      Dummy_Action        : Visit_Action;
      Traverse_Result     : W_Object;

      Files         : W_Template_Instance_Vectors.Vector;
      Output        : W_Template_Instance_Vectors.Vector;
      File_Template : T_Template;
      Out_Template  : T_Template;
   begin
      File_Template :=
        T_Template
          (Resolve_Module_By_Name ("standard").Children_Indexed.Element
             ("file"));

      Out_Template :=
        T_Template
          (Resolve_Module_By_Name ("standard").Children_Indexed.Element
             ("out"));

      while Templates_To_Traverse.Length > 0
        or else Deferred_Commands.Length > 0
      loop
         declare
            Next_Iteration : W_Template_Instance_Vectors.Vector;
         begin
            Next_Iteration.Move (Templates_To_Traverse);
            Templates_To_Traverse.Clear;

            --  Reset the visitor id - we're on the main iteration, id is 0.

            for Created_Template of Next_Iteration loop
               A_Template_Instance := W_Template_Instance (Created_Template);

               if Instance_Of
                   (T_Template (A_Template_Instance.Defining_Entity),
                    File_Template)
               then
                  Files.Append (A_Template_Instance);
               elsif Instance_Of
                   (T_Template (A_Template_Instance.Defining_Entity),
                    Out_Template)
               then
                  Output.Append (A_Template_Instance);
               end if;
            end loop;

            --  Analyzed all defered commands that are valid

            declare
               Next_Deferred : Deferred_Command_Vectors.Vector;
            begin
               for D of Deferred_Commands loop
                  if not Handle_Defered_Command (D) then
                     Next_Deferred.Append (D);
                  end if;
               end loop;

               --  TODO: This is incorrect, additional deferred commands may
               --  have been created above and will be ignored here.
               Deferred_Commands.Move (Next_Deferred);
            end;

            Current_Visitor_Id := 0;

            for T of Next_Iteration loop
               --  The newly created wrappers need to be analyzed in order of
               --  creation. So we're not using the traverse function anymore,
               --  but instead just go through the list.

               Dummy_Action := Analyze_Visitor (T, Traverse_Result);
            end loop;
         end;
      end loop;

      for T of Output loop
         declare
            Content_Object : W_Object;
         begin
            Push_Frame (Wrapping.Semantic.Analysis.Root);

            if not T.Push_Value ("content") then
               Error ("'content' component not found in file template");
            end if;

            Content_Object := Pop_Object;

            Push_Buffer_Cursor;

            declare
               Slice : Buffer_Slice;
            begin
               Slice := Content_Object.Write_String;

               Put
                 (Buffer.Str
                    (Slice.First.Offset .. Slice.Last.Offset));
            end;

            Pop_Buffer_Cursor;

            Pop_Frame;
         end;
      end loop;

      for T of Files loop
         declare
            Path_Object    : W_Object;
            Content_Object : W_Object;
            Output_File    : File_Type;
         begin
            Push_Frame (Wrapping.Semantic.Analysis.Root);

            if not T.Push_Value ("path") then
               Error ("'path' component not found in file template");
            end if;

            Path_Object := Pop_Object;

            if not T.Push_Value ("content") then
               Error ("'content' component not found in file template");
            end if;

            Content_Object := Pop_Object;

            Push_Buffer_Cursor;

            declare
               Slice : Buffer_Slice;
            begin
               Slice := Path_Object.Write_String;

               Create
                 (Output_File,
                  Out_File,
                  To_String (Buffer.Str
                    (Slice.First.Offset .. Slice.Last.Offset)));
            end;

            Pop_Buffer_Cursor;

            Push_Buffer_Cursor;

            declare
               Slice : Buffer_Slice;
            begin
               Slice := Content_Object.Write_String;
               Put (Output_File,
                    Buffer.Str
                      (Slice.First.Offset .. Slice.Last.Offset));
            end;

            Pop_Buffer_Cursor;

            Close (Output_File);

            Pop_Frame;
         end;
      end loop;
   end Analyze_Templates;

   -------------------------
   -- Evaluate_Expression --
   -------------------------

   function Evaluate_Expression (Expr : T_Expr) return W_Object is
   begin
      Evaluate_Expression (Expr);

      return Pop_Object;
   end Evaluate_Expression;

   -------------------------
   -- Evaluate_Expression --
   -------------------------

   procedure Evaluate_Expression (Expr : T_Expr) is
      --  Some expression need to run the outer object callback. For example:
      --    match a
      --  a needs to match with It. Other do not. For example in:
      --     match has'a
      --  we need to not match (has'a) expression. Similarly, in :
      --     match a or b
      --  we need to match individually a and b, but not the expression (a or
      --  b), otherwise it wouldn't be possible to write something like:
      --     match has'a or has'b
      --  (has would discusonnect the outer match, but the overall expression
      --  would match again).
      Run_Outer_Callback : Boolean := True;

      Do_Pop_Frame_Context : Boolean := False;
   begin
      Push_Error_Location (Expr.Node);

      case Expr.Kind is
         when Template_Match_Capture =>
            declare
               Captured_Name : Text_Type :=
                 Expr.Node.As_Match_Capture.F_Captured.Text;
               Previous_Value : W_Object;
            begin
               --  This expression captures the result of the underlying
               --  expression and lets its value pass through.

               --  First, save any previous name capture for restoration, and
               --  store the new one.

               Push_Frame_Context;
               Top_Frame.Top_Context.Name_Captured :=
                 To_Unbounded_Text (Captured_Name);

               if Top_Frame.Symbols.Contains (Captured_Name) then
                  Previous_Value := Top_Frame.Symbols.Element (Captured_Name);
               end if;

               Evaluate_Expression (Expr.Match_Capture_Expr);

               if Top_Frame.Data_Stack.Last_Element /= Match_False then
                  Include_Symbol (Captured_Name, Top_Object);
               else
                  --  For early reference, that name may have already been
                  --  captured. If we eneded up not having a match, it needs
                  --  to be removed, or replaced by the previous value.

                  if Previous_Value /= null then
                     Include_Symbol (Captured_Name, Previous_Value);
                  elsif Top_Frame.Symbols.Contains (Captured_Name) then
                     Top_Frame.Symbols.Delete (Captured_Name);
                  end if;
               end if;

               Pop_Frame_Context;
               Run_Outer_Callback := False;
            end;

         when Template_Selector =>
            declare
               Suffix : T_Expr_Vectors.Vector;
            begin
               Handle_Selector (Expr, Suffix);
            end;

            --  Never match the result of a selection. Matching happened in
            --  Handle_Selector, when evaluating the right operand. At this
            --  stage, we may also not be in the right match mode anymore
            --  (e.g we don't know if we match a reference or a call result).
            Run_Outer_Callback := False;

         when Template_Binary_Expr =>
            --  The convention for "and" and "or" binary operators is to push
            --  to the stack the last object that matched, otherwise false.
            --  This allows to capture that object later on, which can be
            --  useful for example if that object is a newly allocated one.

            declare
               Left, Right : W_Object;
            begin
               case Expr.Node.As_Binary_Expr.F_Op.Kind is
                  when Template_Operator_And =>
                     Left := Evaluate_Expression (Expr.Binary_Left);

                     if Left /= Match_False then
                        Right := Evaluate_Expression (Expr.Binary_Right);

                        if Right /= Match_False then
                           Push_Object (Right);
                        else
                           Push_Match_False;
                        end if;
                     else
                        Push_Match_False;
                     end if;

                     Run_Outer_Callback := False;
                  when Template_Operator_Or =>
                     Left := Evaluate_Expression (Expr.Binary_Left);

                     if Left /= Match_False then
                        Push_Object (Left);
                     else
                        Right := Evaluate_Expression (Expr.Binary_Right);

                        if Right /= Match_False then
                           Push_Object (Right);
                        else
                           Push_Match_False;
                        end if;
                     end if;

                     Run_Outer_Callback := False;
                  when Template_Operator_Amp =>
                     Push_Frame_Context_No_Outer;
                     Left  := Evaluate_Expression (Expr.Binary_Left);
                     Right := Evaluate_Expression (Expr.Binary_Right);
                     Pop_Frame_Context;

                     if Left.Dereference.all in W_Text_Expression_Type'Class
                       and then Right.Dereference.all in
                         W_Text_Expression_Type'Class
                     then
                        declare
                           Container : W_Text_Vector := new W_Text_Vector_Type;
                        begin
                           Container.A_Vector.Append (Left);
                           Container.A_Vector.Append (Right);

                           Push_Object (Container);
                        end;
                     else
                        declare
                           Container : W_Vector := new W_Vector_Type;
                        begin
                           Container.A_Vector.Append (Left);
                           Container.A_Vector.Append (Right);

                           Push_Object (Container);
                        end;
                     end if;
                  when Template_Operator_Plus | Template_Operator_Minus |
                    Template_Operator_Multiply | Template_Operator_Divide |
                    Template_Operator_Eq | Template_Operator_Neq |
                    Template_Operator_Gt | Template_Operator_Lt |
                    Template_Operator_Gte | Template_Operator_Lte =>

                     Push_Frame_Context_No_Outer;
                     Handle_Arithmetic_Operator (Expr);
                     Pop_Frame_Context;

                  when others =>
                     Error ("unexpected operator");

               end case;
            end;

         when Template_Unary_Expr =>
            Push_Frame_Context_No_Pick;

            declare
               Right : W_Object :=
                 Evaluate_Expression (Expr.Unary_Right).Dereference;
            begin
               if Expr.Node.As_Unary_Expr.F_Op.Kind = Template_Operator_Not
               then
                  if Right = Match_False then
                     Push_Match_True (Top_Object.Dereference);
                  else
                     Push_Match_False;
                  end if;
               end if;
            end;

            Pop_Frame_Context;

         when Template_Literal =>
            if Expr.Node.Text = "true" then
               Push_Match_True (Top_Object);
            elsif Expr.Node.Text = "false" then
               Push_Match_False;
            else
               Error ("unkown literal '" & Expr.Node.Text & "'");
            end if;

         when Template_Token_Identifier | Template_Identifier =>
            Push_Frame_Context_No_Outer;
            Handle_Identifier (Expr.Node);
            Pop_Frame_Context;

         when Template_Number =>
            Push_Object (W_Object'(new W_Integer_Type'(Value => Expr.Number)));

         when Template_Str =>
            Push_Frame_Context_No_Outer;
            Evaluate_String (Expr);
            Pop_Frame_Context;

            if Expr.Str_Kind = String_Regexp then
               --  If we wanted a regexp, pop the object on the stack and
               --  replace is with a regexp wrapper.
               Push_Object
                 (W_Object'(new W_Regexp_Type'(Value => Pop_Object)));
            end if;

         when Template_Call_Expr =>
            Push_Frame_Context_No_Pick;
            Handle_Call (Expr);
            Pop_Frame_Context;

            --  Prepare the matching context for the resulting value. As we're
            --  on a call match, we can change the context without pushing /
            --  popping (there's nothing else).

            if Top_Frame.Top_Context.Match_Mode = Match_Ref_Default then
               Top_Frame.Top_Context.Match_Mode := Match_Call_Default;
            end if;

         when Template_Defer_Expr =>
            declare
               Deferred_Expr : W_Deferred_Expr := new W_Deferred_Expr_Type;
            begin
               Capture_Deferred_Environment (Deferred_Expr, Expr);
               Push_Object (Deferred_Expr);

               Pop_Error_Location;
            end;

         when Template_New_Expr =>
            if Top_Frame.Top_Context.Allocate_Callback /= null then
               Handle_New (Expr.Tree);
            else
               Push_Match_False;
            end if;

            Push_Frame_Context;
            Top_Frame.Top_Context.Match_Mode := Match_Has;
            Do_Pop_Frame_Context             := True;

         when Template_At_Ref =>
            if Top_Frame.Top_Context.Left_Value = null then
               Error ("no left value available in this context");
            else
               Push_Object (Top_Frame.Top_Context.Left_Value);
            end if;

         when Template_Qualified_Match =>
            --  We are on an expression like has (something) or is (something).
            --  Specify the kind of match we need to make, which will override
            --  the default.

            if Top_Frame.Top_Context.Match_Mode = Match_None then
               Error
                 ("qualified match operators only available in match context");
            end if;

            Push_Frame_Context;
            Top_Frame.Top_Context.Outer_Expr_Callback :=
              Outer_Expression_Match'Access;

            if Expr.Node.As_Qualified_Match.F_Op = Template_Operator_Is then
               Top_Frame.Top_Context.Match_Mode := Match_Is;
            else
               Top_Frame.Top_Context.Match_Mode := Match_Has;
            end if;

            Evaluate_Expression (Expr.Qualified_Match_Expr);

            Pop_Frame_Context;

            Run_Outer_Callback := False;

         when Template_Match_Expr =>
            Push_Frame_Context_No_Pick;
            Push_Match_Result (Top_Object, Expr.Match_Match_Expr);

            if Pop_Object /= Match_False then
               Evaluate_Expression (Expr.Match_Pick_Expr);
            elsif Expr.Match_Else_Expr /= null then
               Evaluate_Expression (Expr.Match_Else_Expr);
            else
               Push_Match_False;
            end if;

            Pop_Frame_Context;

         when others =>
            Error
              ("unexpected expression node kind: '" &
               Expr.Node.Kind'Wide_Wide_Image & "'");
      end case;

      if Run_Outer_Callback
        and then Top_Frame.Top_Context.Outer_Expr_Callback /= null
      then
         Top_Frame.Top_Context.Outer_Expr_Callback.all;
      end if;

      if Do_Pop_Frame_Context then
         Pop_Frame_Context;
      end if;

      Pop_Error_Location;
   end Evaluate_Expression;

   ----------------------------
   -- Analyze_Replace_String --
   ----------------------------

   Expression_Unit_Number : Integer := 1;

   ---------------------
   -- Evaluate_String --
   ---------------------

   procedure Evaluate_String
     (Expr : T_Expr;
      On_Group : access procedure (Index : Integer; Value : W_Object) := null;
      On_Expression : access procedure (Expr : T_Expr) := null)
   is
      Result : W_Text_Vector := new W_Text_Vector_Type;

      -----------------
      -- Append_Text --
      -----------------

      procedure Append_Text (Text : Text_Type) is
      begin
         Result.A_Vector.Append (W_Object (To_W_String (Text)));
      end Append_Text;

      --------------
      -- On_Error --
      --------------

      procedure On_Error
        (Message : Text_Type; Filename : String; Loc : Source_Location)
      is
      begin
         Push_Error_Location
           (Expr.Node.Unit.Get_Filename, Start_Sloc (Expr.Node.Sloc_Range));

         Put_Line (To_Text (Get_Sloc_Str) & ": " & Message);

         raise Wrapping_Error;
      end On_Error;

      Prev_Error : Error_Callback_Type;
   begin
      Prev_Error     := Error_Callback;
      Error_Callback := On_Error'Unrestricted_Access;

      Push_Frame_Context_No_Match;
      Top_Frame.Top_Context.Is_Root_Selection := True;

      for Str of Expr.Str loop
         case Str.Kind is
            when Str_Kind =>
               Result.A_Vector.Append (W_Object (To_W_String (Str.Value)));
            when Expr_Kind =>
               if On_Expression /= null then
                  On_Expression.all (Str.Expr);
               else
                  Evaluate_Expression (Str.Expr);

                  if Expr.Str_Kind = String_Indent then
                     Result.A_Vector.Append
                       (new W_Text_Reindent_Type'
                          (Indent => Str.Indent, Content => Pop_Object));
                  else
                     Result.A_Vector.Append (W_Object (Pop_Object));
                  end if;
               end if;
            when Group_Kind =>
               declare
                  Position : Integer := Str.Group_Number;
                  Value    : W_Object;
               begin
                  for C of Top_Frame.Group_Sections loop
                     if Integer (C.Groups.Length) < Position then
                        Position := Position - Integer (C.Groups.Length);
                     else
                        Value := C.Groups.Element (Position);
                        exit;
                     end if;
                  end loop;

                  if Value = null then
                     Error
                       ("cannot find group " &
                        Integer'Wide_Wide_Image (Str.Group_Number));
                  end if;

                  if On_Group /= null then
                     On_Group.all (Str.Group_Number, Value);
                  else
                     Push_Buffer_Cursor;

                     declare
                        Slice : Buffer_Slice;
                     begin
                        Slice := Value.Write_String;
                        Append_Text
                          (Buffer.Str
                             (Slice.First.Offset .. Slice.Last.Offset));
                     end;

                     Pop_Buffer_Cursor;
                  end if;
               end;
         end case;
      end loop;

      if Expr.Str_Kind = String_Indent then
         Push_Object
           (W_Object'
              (new W_Text_Reindent_Type'
                 (Indent => 0, Content => W_Object (Result))));
      else
         Push_Object (Result);
      end if;

      Error_Callback := Prev_Error;
      Pop_Frame_Context;
   end Evaluate_String;

   ----------------------------
   -- Push_Global_Identifier --
   ----------------------------

   function Push_Global_Identifier (Name : Text_Type) return Boolean with
      Post => Top_Frame.Data_Stack.Length'Old =
      (if Push_Global_Identifier'Result then Top_Frame.Data_Stack.Length - 1
       else Top_Frame.Data_Stack.Length)
   is
      A_Module         : T_Module;
      Tentative_Symbol : W_Object;
      Semantic_Entity  : T_Entity;
   begin
      if Name = "it" then
         Push_Object (Get_Implicit_It);

         return True;
      elsif Name = "text" then
         --  We're on an object to text conversion. Set the runtime object.
         --  When running the call, the link with the undlerlying expression
         --  will be made.
         Push_Intrinsic_Function (null, Call_Convert_To_Text'Access);

         return True;
      elsif Name = "string" then
         --  We're on an object to string conversion. Set the text object. When
         --  running the call, the actual text value will be computed and put
         --  in the object.

         Push_Intrinsic_Function (null, Call_Convert_To_String'Access);

         return True;
      elsif Name = "normalize_ada_name" then
         Push_Intrinsic_Function (null, Call_Normalize_Ada_Name'Access);
         return True;
      elsif Name = "replace_text" then
         Push_Intrinsic_Function (null, Call_Replace_Text'Access);
         return True;
      elsif Name = "to_lower" then
         Push_Intrinsic_Function (null, Call_To_Lower'Access);
         return True;
      elsif Name = "reindent" then
         Push_Intrinsic_Function (null, Call_Reindent'Access);
         return True;
      end if;

      --  Check in the dynamic symols in the frame

      Tentative_Symbol := Get_Visible_Symbol (Top_Frame.all, Name);

      A_Module := Get_Module (Top_Frame.all);

      --  Check if the current module is the name we're looking for

      if To_Text (A_Module.Name) = Name then
         Push_Object
           (W_Object'
              (new W_Static_Entity_Type'(An_Entity => T_Entity (A_Module))));

         return True;
      end if;

      --  Check in the static symbols in the module

      if A_Module.Children_Indexed.Contains (Name) then
         if Tentative_Symbol = null then
            Semantic_Entity := A_Module.Children_Indexed (Name);

            if Semantic_Entity.all in T_Template_Type'Class then
               Tentative_Symbol :=
                 new W_Static_Entity_Type'(An_Entity => Semantic_Entity);
            elsif Semantic_Entity.all in T_Function_Type'Class then
               Tentative_Symbol :=
                 new W_Function_Type'
                   (A_Function => T_Function (Semantic_Entity));
            end if;
         else
            Error
              ("can't reference " & Name & ", multiple definitions hiding");
         end if;
      end if;

      --  Check in the imported symbols in the module

      for Imported of A_Module.Imported_Modules loop
         if Imported.Children_Indexed.Contains (Name) then
            if Tentative_Symbol = null then
               Semantic_Entity := Imported.Children_Indexed (Name);

               if Semantic_Entity.all in T_Template_Type'Class then
                  Tentative_Symbol :=
                    new W_Static_Entity_Type'(An_Entity => Semantic_Entity);
               elsif Semantic_Entity.all in T_Function_Type'Class then
                  Tentative_Symbol :=
                    new W_Function_Type'
                      (A_Function => T_Function (Semantic_Entity));
               end if;
            else
               Error
                 ("can't reference " & Name & ", multiple definitions hiding");
            end if;
         end if;
      end loop;

      --  Check in the namesaces symbols

      if Wrapping.Semantic.Analysis.Root.Children_Indexed.Contains (Name) then
         if Tentative_Symbol = null then
            Semantic_Entity :=
              Wrapping.Semantic.Analysis.Root.Children_Indexed.Element (Name);

            if Semantic_Entity.all in T_Namespace_Type'Class
              or else Semantic_Entity.all in T_Module_Type'Class
            then
               Tentative_Symbol :=
                 new W_Static_Entity_Type'(An_Entity => Semantic_Entity);
            end if;
         else
            Error
              ("can't reference " & Name & ", multiple definitions hiding");
         end if;
      end if;

      if Tentative_Symbol = null then
         return False;
      else
         Push_Object (Tentative_Symbol);
         return True;
      end if;
   end Push_Global_Identifier;

   ------------------------------
   -- Handle_Global_Identifier --
   ------------------------------

   procedure Handle_Global_Identifier (Name : Text_Type) is
   begin
      if not Push_Global_Identifier (Name) then
         Error ("can't find global reference to '" & Name & "'");
      end if;
   end Handle_Global_Identifier;

   -----------------------
   -- Handle_Identifier --
   -----------------------

   procedure Handle_Identifier (Node : Template_Node'Class) is
      procedure Handle_Language_Entity_Selection with
         Post => Top_Frame.Data_Stack.Length =
         Top_Frame.Data_Stack.Length'Old + 1;

         ------------------------------------
         -- Handle_Static_Entity_Selection --
         ------------------------------------

      procedure Handle_Static_Entity_Selection with
         Post => Top_Frame.Data_Stack.Length =
         Top_Frame.Data_Stack.Length'Old + 1
      is
         Name : Text_Type := Node.Text;
      begin
         --  TODO: We probably don't need a specific function here anymore.

         if not Top_Object.Push_Value (Name) then
            if Top_Frame.Top_Context.Match_Mode /= Match_None then
               Push_Match_False;
            else
               Error ("'" & Node.Text & "' not found");
            end if;
         end if;
      end Handle_Static_Entity_Selection;

      --------------------------------------
      -- Handle_Language_Entity_Selection --
      --------------------------------------

      procedure Handle_Language_Entity_Selection is
         Name : Text_Type := Node.Text;

         Implicit_It     : W_Object;
         Found_It_Entity : Boolean;
         Prefix_Entity   : W_Object;
      begin
         --  We're resolving a reference to an entity

         if Top_Frame.Top_Context.Is_Root_Selection then
            --  If we're on the implicit entity, then first check if there's
            --  some more global identifier overriding it.

            if Push_Global_Identifier (Node.Text) then

               return;
            end if;

            --  Retreive the entity from implicit It. If Implicit new exist,
            --  we need to also attempt at retreiving its value. We'll return
            --  either the entity coming from one of the two, or raise an error
            --  if both contain such name.

            Implicit_It := Get_Implicit_It;

            Found_It_Entity := Implicit_It.Push_Value (Name);

            if Found_It_Entity then
               return;
            end if;

            if Top_Frame.Top_Context.Match_Mode /= Match_None then
               Push_Match_False;
               return;
            else
               Error ("'" & Node.Text & "' not found");
            end if;
         else
            --  We're on an explicit name. Push the result.

            Prefix_Entity := Top_Object.Dereference;

            if Prefix_Entity = Match_False then
               Error ("prefix not found");
            end if;

            if Prefix_Entity.Push_Value (Name) then
               --  We found a component of the entity and it has been pushed
               return;
            else
               if Top_Frame.Top_Context.Match_Mode /= Match_None then
                  Push_Match_False;
                  return;
               else
                  Error ("'" & Name & "' component not found");
               end if;
            end if;
         end if;
      end Handle_Language_Entity_Selection;

      Top : W_Object;
   begin
      if Top_Frame.Data_Stack.Length /= 0 then
         Top := Top_Object.Dereference;

         if Top.all in W_Static_Entity_Type'Class then
            Handle_Static_Entity_Selection;
         else
            Handle_Language_Entity_Selection;
         end if;
      else
         Handle_Global_Identifier (Node.Text);
      end if;
   end Handle_Identifier;

   --------------------------
   -- Handle_Template_Call --
   --------------------------

   function Handle_Template_Call
     (A_Template_Instance : W_Template_Instance; Args : T_Arg_Vectors.Vector)
      return Visit_Action
   is
      --  If not already evaluated, parameter evaluation for templates goes as
      --  follows: (1) actual expressions for parameters on a template call are
      --  stored
      --      in the current frame.
      --  (2) when variables are encountered in the following sequences, they
      --      are first evaluated, then
   --      (a) if there's an expression available the ordered expression list,
      --          it will be evaluated
      --      (b) otherwise, if there's an experssion of that name in the list,
      --          it will be evaluated
      --      This evaluation is done in the context of the parent frame, the
      --      one where the expression is written, not the top frame which is
      --      the current frame of the template.
      --  (3) At the end of the template call, if there are still parameters
      --      not evaluated, an error is thrown.
      --  The following allows this to work:
      --
      --     template T do
      --        var V : text => "something";
      --     end;
      --
      --     wrap T (V => @ & " and something else");
      --
   --   Wrapping T this way would evaluate V as "something and something else".
      --
      --   If the template has already been evaluated, then we only update its
      --   variables.

      ---------------------
      -- Store_Parameter --
      ---------------------

      procedure Store_Parameter
        (Name : Text_Type; Position : Integer; Value : T_Expr)
      is
      begin
         if Name = "" then
            Top_Frame.Template_Parameters_Position.Append (Value);
         else
            Top_Frame.Template_Parameters_Names.Insert (Name, Value);
         end if;
      end Store_Parameter;

      ----------------------
      -- Update_Parameter --
      ----------------------

      procedure Update_Parameter
        (Name : Text_Type; Position : Integer; Value : T_Expr)
      is
         Ref : W_Reference;
      begin
         if Name = "" then
            if Position > A_Template_Instance.Ordered_Variables.Last_Index then
               Error
                 ("object does not contain variable at position" &
                  Integer'Wide_Wide_Image (Position));
            end if;

            Ref := A_Template_Instance.Ordered_Variables.Element (Position);
         else
            if not A_Template_Instance.Indexed_Variables.Contains (Name) then
               Error ("object does not contain variable '" & Name & "'");
            end if;

            Ref := A_Template_Instance.Indexed_Variables.Element (Name);
         end if;

         Push_Frame_Context;
         Top_Frame.Top_Context.Left_Value := Ref.Value;
         Evaluate_Expression (Value);
         Ref.Value := Pop_Object;

         Pop_Frame_Context;
      end Update_Parameter;

      ------------------------------------
      -- Handle_Template_Call_Recursive --
      ------------------------------------

      procedure Handle_Template_Call_Recursive (A_Template : T_Template) is
      begin
         if A_Template.Extends /= null then
            Handle_Template_Call_Recursive (A_Template.Extends);
         end if;

         if A_Template.Program /= null then
            Handle_Command_Front (A_Template.Program);
         end if;
      end Handle_Template_Call_Recursive;

      Visit_Result : aliased Visit_Action := Unknown;
   begin
      if A_Template_Instance.Is_Evaluated then
         Handle_Call_Parameters (Args, Update_Parameter'Access);

         return Into;
      else
         A_Template_Instance.Is_Evaluated := True;

         Handle_Call_Parameters (Args, Store_Parameter'Access);

         Push_Frame (A_Template_Instance.Defining_Entity);
         Push_Implicit_It (A_Template_Instance);
         Top_Frame.Top_Context.Visit_Decision := Visit_Result'Unchecked_Access;
         Top_Frame.Current_Template := W_Object (A_Template_Instance);

         if A_Template_Instance.Defining_Entity.Full_Name = "standard.root"
         then
            Apply_Wrapping_Program
              (A_Template_Instance.Origin, Wrapping.Semantic.Analysis.Root);
         else
            Handle_Template_Call_Recursive
              (T_Template (A_Template_Instance.Defining_Entity));
         end if;

         if Parent_Frame.Template_Parameters_Position.Length > 0 then
            Error ("too many parameters");
         elsif Parent_Frame.Template_Parameters_Names.Length > 0 then
            Error
              ("no parameter '" &
               Parent_Frame.Template_Parameters_Names.First_Key &
               "' found for template");
         end if;

         Pop_Object;
         Pop_Frame;

         if Visit_Result = Unknown then
            return Into;
         else
            return Visit_Result;
         end if;
      end if;
   end Handle_Template_Call;

   -----------------
   -- Handle_Call --
   -----------------

   procedure Handle_Call (Expr : T_Expr) is
      Called : W_Object;
   begin
      Push_Frame_Context;

      --  If we're matching, currently under the default ref mode, then move to
      --  the default call mode.

      if Top_Frame.Top_Context.Match_Mode = Match_Ref_Default then
         Top_Frame.Top_Context.Match_Mode := Match_Call_Default;
      end if;

      --  When evaluating the name of the function, we need to verify that it
      --  matches the current object. For example in:
      --     match w_SomeTemplate ()
      --  we may find a reference to w_SomeTemplate as a static entity, but it
      --  may not match the current object (it in the example above). This will
      --  be checked by the Outer_Expression_Match callback below. Note that
      --  intrinsinc and custom functions will always match, so that e.g.
      --     match to_lower (some_value)
      --  will always go through.

      Top_Frame.Top_Context.Outer_Expr_Callback :=
        Outer_Expression_Match'Access;

      Called := Evaluate_Expression (Expr.Called).Dereference;

      Pop_Frame_Context;

      --  If the called identifier didn't match, we either just push a match
      --  false if we're in a matching section, or raise an error. Otherwise,
      --  execute the call on the retreived function

      if Called = Match_False then
         if Top_Frame.Top_Context.Match_Mode /= Match_None then
            Push_Match_False;
         else
            Error ("call not matching context");
         end if;
      else
         Called.Push_Call_Result (Expr.Args);

         if not Called.Is_Generator then
            --  If the called subprogram is a generator, then it will have
            --  called the yeild callback upon value generation. Otherwise,
            --  call yield on the one returned value.

            Call_Yield;
         end if;
      end if;
   end Handle_Call;

   -----------------------------
   -- Compute_Selector_Suffix --
   -----------------------------

   procedure Compute_Selector_Suffix (Suffix : T_Expr_Vectors.Vector) with
      Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old + 1
   is
      Terminal : T_Expr;

      Has_Prev          : Boolean := False;
      Suffix_Expression : T_Expr;
   begin
      if Suffix.Length = 0 then
         Push_Match_False;
         return;
      end if;

      Terminal := Suffix.Last_Element;

      --  The left part of a selector may have calls. In this case, these calls
      --  are unrelated to the value that is possibly being captured. E.g. in:
      --     a: b ().c
      --  b () value is not being captured in a. In order to respect that, the
      --  current captured name is removed when processing the left part of
      --  the selector. Similarily, we only fold on the target of the fold.
      --  For example, in:
      --     child ().child ().fold ()
      --  the first child is a selecing the first match, the second is folded.
      --  Note that the left end of an expression is never matching with the
      --  outer context, hence setting the match flag to none.

      Push_Frame_Context_No_Match;
      Top_Frame.Top_Context.Name_Captured  := To_Unbounded_Text ("");
      Top_Frame.Top_Context.Yield_Callback := null;

      for I in Suffix.First_Index .. Suffix.Last_Index - 1 loop
         Suffix_Expression := Suffix.Element (I);

         Evaluate_Expression (Suffix_Expression);

         if Has_Prev then
            Delete_Object_At_Position (-2);
         end if;

         Has_Prev                                := True;
         Top_Frame.Top_Context.Is_Root_Selection := False;
      end loop;

      Pop_Frame_Context;
      Push_Frame_Context;
      Top_Frame.Top_Context.Is_Root_Selection := False;

      --  Run the terminal separately. In particular in the case of:
      --     x.y.z.child().all()
      --  while x.y.z needs to be called outside of the yield context, child
      --  () need to be called with the frame context set by all which set in
      --  particular yiekd context to true.

      Evaluate_Expression (Terminal);

      if Has_Prev then
         Delete_Object_At_Position (-2);
      end if;

      Pop_Frame_Context;
   end Compute_Selector_Suffix;

   ---------------------
   -- Handle_Selector --
   ---------------------

   procedure Handle_Selector
     (Expr : T_Expr; Suffix : in out T_Expr_Vectors.Vector)
   is
   begin
      --  In a selector, we compute the left object, build the right expression
      --  based on the left object, remove the left object
      --  and then put the result on the left object on the stack.

      if Expr.Selector_Left = null then
         --  This can happen in particualr with the rule "dotted_identifier".
         --  TODO: We should look at getting rid of this special case.

         Push_Frame_Context;
         Top_Frame.Top_Context.Is_Root_Selection := True;
         Evaluate_Expression (Expr.Selector_Right);
         Pop_Frame_Context;
      elsif Expr.Selector_Right.Kind in Template_All_Expr then
         Handle_All (Expr, Suffix);
      elsif Expr.Selector_Right.Kind in Template_Fold_Expr then
         Handle_Fold (Expr, Suffix);
      elsif Expr.Selector_Right.Kind in Template_Filter_Expr then
         Handle_Filter (Expr, Suffix);
      elsif Expr.Selector_Left.Kind in Template_Selector then
         Suffix.Prepend (Expr.Selector_Right);
         Handle_Selector (Expr.Selector_Left, Suffix);
      else
         Suffix.Prepend (Expr.Selector_Right);
         Suffix.Prepend (Expr.Selector_Left);

         Compute_Selector_Suffix (Suffix);
      end if;
   end Handle_Selector;

   -----------------
   -- Handle_Fold --
   -----------------

   procedure Handle_Fold (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) is

      Fold_Expr : T_Expr := Selector.Selector_Right;

      Is_First : Boolean := True;

      Current_Expression : W_Object;

      --------------------
      -- Yield_Callback --
      --------------------

      procedure Yield_Callback is
      begin
         if Is_First then
            Is_First := False;
         elsif Fold_Expr.Separator /= null then
            Push_Frame_Context_Parameter;
            Top_Frame.Top_Context.Left_Value := Current_Expression;
            Evaluate_Expression (Fold_Expr.Separator);
            Current_Expression := Pop_Object;
            Pop_Frame_Context;
         end if;

         Push_Frame_Context;
         Top_Frame.Top_Context.Left_Value := Current_Expression;
         Evaluate_Expression (Fold_Expr.Combine);
         Current_Expression := Top_Object;
         Pop_Frame_Context;
      end Yield_Callback;

   begin
      --  Inside the folded expression, we need to go back to a situation where
      --  It is top of the stack, as name can refer to the implicit It. Re push
      --  this value

      Push_Frame_Context_Parameter;
      Push_Implicit_It (Get_Implicit_It);
      Current_Expression := Evaluate_Expression (Fold_Expr.Default);

      --  If the name captured is not null, provide its value here. This allows
      --  two equivalent stypes for fold:
      --     x: child ().fold ("", x & something)
      --  or
      --     child ().fold (x: "", x: (x & something))
      --  which is consistent with the overall way capture works.
      if Top_Frame.Top_Context.Name_Captured /= "" then
         Include_Symbol
           (To_Text (Top_Frame.Top_Context.Name_Captured), Current_Expression);
      end if;

      Pop_Object;
      Pop_Frame_Context;

      Push_Frame_Context;
      Top_Frame.Top_Context.Yield_Callback :=
        Yield_Callback'Unrestricted_Access;
      Top_Frame.Top_Context.Match_Mode          := Match_None;
      Top_Frame.Top_Context.Outer_Expr_Callback := null;
      Top_Frame.Top_Context.Is_Root_Selection   := True;

      Evaluate_Expression (Selector.Selector_Left);

      --  The prefix will have pushed its own result which needs to be
      --  disregarded at this stage. Push the fold result instead.

      Pop_Object;
      Push_Object (Current_Expression);

      Pop_Frame_Context;

      if Suffix.Length > 0 then
         Compute_Selector_Suffix (Suffix);
         Delete_Object_At_Position (-2);
      elsif Top_Frame.Top_Context.Outer_Expr_Callback /= null then
         Push_Frame_Context;

         if Top_Frame.Top_Context.Match_Mode = Match_Ref_Default then
            Top_Frame.Top_Context.Match_Mode := Match_Call_Default;
         end if;

         Top_Frame.Top_Context.Outer_Expr_Callback.all;

         Pop_Frame_Context;
      end if;
   end Handle_Fold;

   -------------------
   -- Handle_Filter --
   -------------------

   procedure Handle_Filter (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector)
   is
      Filtered_Expr   : T_Expr := Selector.Selector_Right.Filter_Expr;
      Prefix_Function : T_Expr;

      Object_Mode : Boolean;

      Match_Mode : Match_Kind;

      ---------------
      -- Generator --
      ---------------

      procedure Generator (Expr : T_Expr) is

         Original_Yield : Yield_Callback_Type :=
           Top_Frame.Top_Context.Yield_Callback;

         --------------------
         -- Yield_Callback --
         --------------------

         procedure Yield_Callback is
         begin
            Push_Implicit_It (Top_Object);
            Push_Match_Result (Top_Object, Expr);
            Delete_Object_At_Position (-2);

            if Top_Object /= Match_False then
               if Original_Yield /= null then
                  --  We are generating values, calling the original generator.

                  Call_Yield (Original_Yield);
               else
                  --  We're just looking for the first matching value,
                  --  interrupt the current iteration

                  --  ??? SHOULD THAT BE Visit_Decision instead?
                  Parent_Frame.Interrupt_Program := True;
               end if;
            end if;
         end Yield_Callback;

      begin
         Push_Frame_Context_No_Match;
         Top_Frame.Top_Context.Match_Mode := Match_Mode;
         Top_Frame.Top_Context.Yield_Callback :=
           Yield_Callback'Unrestricted_Access;

         --  We may be called from an anchored context. However, this anchor
         --  should not be passed to the prefix, to which we're just getting
         --  values one by one.
         Top_Frame.Top_Context.Regexpr_Anchored := False;

         if Object_Mode then
            --  Calling with a null expression - the expression will be checked
            --  in the Yield callback.

            Top_Object.Generate_Values (null);
         else
            Evaluate_Expression (Prefix_Function);
         end if;

         Pop_Frame_Context;
      end Generator;

      ----------------------
      -- Object_Generator --
      ----------------------

      procedure Object_Generator
        (Node : access W_Object_Type'Class; Expr : T_Expr)
      is
      begin
         Top_Object.Generate_Values (Expr);
      end Object_Generator;

   begin
      if Top_Frame.Top_Context.Match_Mode /= Match_None then
         --  If we enter the filter in any match mode, then we're running a
         --  match operation. The Match_Has filter will be tolerant to prefixes
         --  that don't exist and stack a Match_False instead of an error in
         --  these cases.

         Match_Mode := Match_Has;
      end if;

      Push_Frame_Context_No_Match;

      Top_Frame.Top_Context.Match_Mode := Match_Mode;
      Top_Frame.Top_Context.Is_Root_Selection := True;

      --  A filter expression is about calling the directly prefixing function
      --  several times to find a matching pattern. First identify the inital
      --  value of It and the expression on which the filter is done. In the
      --  case:
      --     X ().filter ()
      --  It is current It, X is the function. In the case:
      --     A.B.X ().filter ()
      --  X is still the function, but A.B needs to be computed to retreive It.

      case Selector.Selector_Left.Kind is
         when Template_Selector =>
            Prefix_Function := Selector.Selector_Left.Selector_Right;
            Evaluate_Expression (Selector.Selector_Left.Selector_Left);

         when others =>
            Prefix_Function := Selector.Selector_Left;
            Push_Object (Top_Object);

      end case;

      --  At this stage, we evaluated the prefix of A.B.X () if any. Now launch
      --  the regexp analysis

      if Prefix_Function.Kind = Template_Identifier then
         --  This is an identifier. Call the generator for the object

         Evaluate_Expression (Prefix_Function);
         Delete_Object_At_Position (-2);
         Object_Mode := True;
      else
         Object_Mode := False;
      end if;

      Evaluate_Generator_Regexp
        (Root      => Top_Object,
         Generator => Generator'Unrestricted_Access,
         Expr      => Filtered_Expr);

      Delete_Object_At_Position (-2);
      Pop_Frame_Context;

      if Suffix.Length > 0 then
         Compute_Selector_Suffix (Suffix);
         Delete_Object_At_Position (-2);
      elsif Top_Frame.Top_Context.Outer_Expr_Callback /= null then
         Push_Frame_Context;

         if Top_Frame.Top_Context.Match_Mode = Match_Ref_Default then
            Top_Frame.Top_Context.Match_Mode := Match_Call_Default;
         end if;

         Top_Frame.Top_Context.Outer_Expr_Callback.all;

         Pop_Frame_Context;
      end if;
   end Handle_Filter;

   ----------------
   -- Handle_All --
   ----------------

   procedure Handle_All (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) is
      Initial_Context : Frame_Context := Top_Frame.Top_Context;

      --------------------
      -- Yield_Callback --
      --------------------

      procedure Yield_Callback is
         Visit_Decision : Visit_Action_Ptr;
      begin
         --  First try to run the all filter if any, and cancel the iteration
         --  if no match.

         if Selector.Selector_Right.All_Match /= null then
            Evaluate_Expression (Selector.Selector_Right.All_Match);

            if Top_Object = Match_False then
               return;
            else
               Pop_Object;
            end if;
         end if;

         Visit_Decision := Top_Frame.Top_Context.Visit_Decision;

         --  Restore the context at this point of the call. This is important
         --  in particular if there was an expansion happening there, e.g.
         --  a.all().b.all().
         Push_Frame_Context (Initial_Context.all);

         --  We still however need to keep control to the same visit iteration
         Top_Frame.Top_Context.Visit_Decision := Visit_Decision;

         --  The outer callback has to be a match check here. If it's a
         --  Outer_Expression_Pick, this is only to be called on the
         --  returning value.
         Top_Frame.Top_Context.Outer_Expr_Callback :=
           Outer_Expression_Match'Access;

         if Suffix.Length > 0 then
            --  If there's a suffix, then compute it to get the value of the
            --  expansion
            Compute_Selector_Suffix (Suffix);
         else
            --  Otherwise, the value is the just the last stacked object.
            --  Dereference it to remove the potential "It" attribute
            Push_Object (Top_Object.Dereference);
         end if;

         --  .all () may itself be in an expression such as .all().fold(). In
         --  this case an expand action is set and needs to be executed.
         Call_Yield (Initial_Context.Yield_Callback);

         if Initial_Context.Outer_Expr_Callback /= null then
            --  Execute the outer action once per run of the suffix, which may
            --  be a Outer_Expression_Pick call.
            Initial_Context.Outer_Expr_Callback.all;
         end if;

         Pop_Frame_Context;
      end Yield_Callback;

   begin
      Push_Frame_Context_No_Match;

      Top_Frame.Top_Context.Yield_Callback :=
        Yield_Callback'Unrestricted_Access;

      Evaluate_Expression (Selector.Selector_Left);

      Pop_Frame_Context;
   end Handle_All;

   ----------------
   -- Handle_New --
   ----------------

   procedure Handle_New (Create_Tree : T_Create_Tree) is

      ----------------------------
      -- Handle_Create_Template --
      ----------------------------

      function Handle_Create_Template
        (New_Tree : T_Create_Tree; Parent : W_Template_Instance)
         return W_Template_Instance
      is
         New_Node : W_Template_Instance;
         Captured : Text_Type := To_Text (New_Tree.Call.Captured_Name);

         --  TODO: Is this necessary?
         Dummy_Action : Visit_Action;
      begin
         Push_Error_Location (New_Tree.Node);
         New_Node :=
           Create_Template_Instance
             (null, T_Template (New_Tree.Call.Reference), True);

         if Captured /= "" then
            Include_Symbol (Captured, W_Object (New_Node));
         end if;

         if Parent = null then
            --  If this is the root of the creation, then we need to signal
            --  this outside. This is needed in particular for constructions
            --  like: child (new (T ())) or child (new ([T(), T()])) where
            --  the root entities need to be connected to the children of
            --  the parent entity.

            --  for the form new (T() []), only the first one needs to be
            --  passed above.

            Top_Frame.Top_Context.Allocate_Callback.all (New_Node);
         else
            Add_Wrapping_Child (Parent, New_Node);
         end if;

         Dummy_Action := Handle_Template_Call (New_Node, New_Tree.Call.Args);
         Pop_Error_Location;

         return New_Node;
      end Handle_Create_Template;

      ------------------------
      -- Handle_Create_Tree --
      ------------------------

      function Handle_Create_Tree
        (A_Tree : T_Create_Tree; Parent : W_Template_Instance)
         return W_Template_Instance
      is
         Main_Node : W_Template_Instance;
         Dummy     : W_Template_Instance;
      begin
         if A_Tree.Call /= null then
            Main_Node := Handle_Create_Template (A_Tree, Parent);

            for C of A_Tree.Subtree loop
               Dummy := Handle_Create_Tree (C, Main_Node);
            end loop;

            return Main_Node;
         else
            --  In the case of new ([T(), T()], every first level will need
            --  to be passed to the above level, and the last one will be the
            --  result of the operator.

            for C of A_Tree.Subtree loop
               Main_Node := Handle_Create_Tree (C, Parent);
            end loop;

            return Main_Node;
         end if;
      end Handle_Create_Tree;

   begin
      Push_Frame_Context_No_Match;
      Push_Allocated_Entity (Handle_Create_Tree (Create_Tree, null));
      Pop_Frame_Context;
   end Handle_New;

   --------------------------------
   -- Handle_Arithmetic_Operator --
   --------------------------------

   procedure Handle_Arithmetic_Operator (Expr : T_Expr) is
      Left, Right     : W_Object;
      Left_I, Right_I : Integer;
      Result          : W_Object;
      Kind : Template_Node_Kind_Type := Expr.Node.As_Binary_Expr.F_Op.Kind;
   begin
      Top_Frame.Top_Context.Match_Mode := Match_Has;

      Left := Evaluate_Expression (Expr.Binary_Left);

      if Left.Dereference.all not in W_Integer_Type'Class then
         Error
           ("Expected integer for left operand, found " &
            Wide_Wide_Expanded_Name (Left.Dereference.all'Tag));
      else
         Left_I := W_Integer (Left.Dereference).Value;
      end if;

      Right := Evaluate_Expression (Expr.Binary_Right);

      if Right.Dereference.all not in W_Integer_Type'Class then
         Error
           ("Expected integer for right operand, found " &
            Wide_Wide_Expanded_Name (Left.Dereference.all'Tag));
      else
         Right_I := W_Integer (Right.Dereference).Value;
      end if;

      case Kind is
         when Template_Operator_Plus | Template_Operator_Minus |
           Template_Operator_Multiply | Template_Operator_Divide =>

            Result :=
              new W_Integer_Type'
                (Value =>
                   (case Expr.Node.As_Binary_Expr.F_Op.Kind is
                      when Template_Operator_Plus     => Left_I + Right_I,
                      when Template_Operator_Minus    => Left_I - Right_I,
                      when Template_Operator_Multiply => Left_I * Right_I,
                      when Template_Operator_Divide   => Left_I / Right_I,
                      when others                     => 0));

         when Template_Operator_Eq | Template_Operator_Neq |
           Template_Operator_Gt | Template_Operator_Lt |
           Template_Operator_Gte | Template_Operator_Lte =>

            if
              (case Expr.Node.As_Binary_Expr.F_Op.Kind is
                 when Template_Operator_Eq  => Left_I = Right_I,
                 when Template_Operator_Neq => Left_I /= Right_I,
                 when Template_Operator_Gt  => Left_I > Right_I,
                 when Template_Operator_Lt  => Left_I < Right_I,
                 when Template_Operator_Gte => Left_I >= Right_I,
                 when Template_Operator_Lte => Left_I <= Right_I,
                 when others                => False)
            then
               Result := Top_Object;
            else
               Result := Match_False;
            end if;

         when others =>
            Error ("unexpected operator");

      end case;

      Push_Object (Result);
   end Handle_Arithmetic_Operator;

   ---------------------
   -- Capture_Closure --
   ---------------------

   function Capture_Closure (Names : Text_Sets.Set) return Closure is
      A_Closure : Closure := new Closure_Type;
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Is_Root_Selection := True;

      for Name of Names loop
         if Push_Global_Identifier (Name) then
            if Top_Object.Dereference.all in W_Static_Entity_Type'Class
              or else Top_Object.Dereference.all in W_Function_Type'Class
            then
               --  We don't capture static references, they can later be
               --  retreived from context. Genreating symbols for them would
               --  also confused name resolution as we would have a symbol and
               --  a statically solvable name.
               Pop_Object;
            elsif Top_Object.all in W_Reference_Type'Class
              and then W_Reference (Top_Object).Is_Implicit_It
            then
               --  We don't want to carry the It property over to the deferred
               --  call, so remove it.

               A_Closure.Captured_Symbols.Insert
                 (Name,
                  new W_Reference_Type'
                    (Value => W_Reference (Pop_Object).Value, others => <>));
            else
               A_Closure.Captured_Symbols.Insert (Name, Pop_Object);
            end if;
         end if;
      end loop;

      A_Closure.Implicit_It   := Get_Implicit_It;
      A_Closure.Lexical_Scope := Top_Frame.Lexical_Scope;
      A_Closure.Temp_Names    := Top_Frame.Temp_Names;

      return A_Closure;
   end Capture_Closure;

   ----------------------------------
   -- Capture_Deferred_Environment --
   ----------------------------------

   procedure Capture_Deferred_Environment
     (Deferred_Expr : W_Deferred_Expr; Expr : T_Expr)
   is
   begin
      Deferred_Expr.A_Closure := Capture_Closure (Expr.Deferred_Closure);
      Deferred_Expr.Expr      := Expr.Deferred_Expr;
   end Capture_Deferred_Environment;

   -----------------------
   -- Run_Deferred_Expr --
   -----------------------

   procedure Run_Deferred_Expr (Deferred_Expr : W_Deferred_Expr_Type) is
      Result : W_Object;
   begin
      Push_Frame (Deferred_Expr.A_Closure);

      Result := Evaluate_Expression (Deferred_Expr.Expr);
      Pop_Frame;
      Push_Object (Result);
   end Run_Deferred_Expr;

   ----------------------------
   -- Outer_Expression_Match --
   ----------------------------

   procedure Outer_Expression_Match is
   begin
      if Top_Frame.Top_Context.Match_Mode not in Match_None | Match_Has then
         --  If we're matching, and we're not forcing the "has" mode, then
         --  check that the initial object we had on the stack matches the
         --  new one.

         if not Top_Frame.Top_Context.Outer_Object.Match_With_Top_Object then
            Pop_Object;
            Push_Match_False;
         end if;
      end if;
   end Outer_Expression_Match;

   ---------------------------
   -- Outer_Expression_Pick --
   ---------------------------

   procedure Outer_Expression_Pick is
   begin
      if Top_Frame.Top_Context.Pick_Callback /= null
        and then Top_Frame.Top_Context.Current_Command.Command_Sequence = null
      then
         --  We are on a final pick expression (not followed by a command
         --  sequence). The Pick_Callback contains what to do with the
         --  picked object.
         Top_Frame.Top_Context.Pick_Callback (Top_Object);
      else
         Handle_Command_Back (Top_Frame.Top_Context.Current_Command);
      end if;
   end Outer_Expression_Pick;

end Wrapping.Runtime.Analysis;
