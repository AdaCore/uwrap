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

with Ada.Tags;                        use Ada.Tags;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Libtemplatelang.Common;   use Libtemplatelang.Common;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Semantic.Analysis;  use Wrapping.Semantic.Analysis;
with Wrapping.Runtime.Commands;   use Wrapping.Runtime.Commands;
with Wrapping.Runtime.Matching;   use Wrapping.Runtime.Matching;
with Wrapping.Runtime.Strings;    use Wrapping.Runtime.Strings;
with Wrapping.Runtime.Objects;    use Wrapping.Runtime.Objects;
with Wrapping.Runtime.Nodes;      use Wrapping.Runtime.Nodes;
with Wrapping.Runtime.Functions;  use Wrapping.Runtime.Functions;
with Wrapping.Runtime.Parameters; use Wrapping.Runtime.Parameters;

package body Wrapping.Runtime.Expressions is

   procedure Handle_Identifier (Node : Template_Node'Class) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   procedure Handle_Call (Expr : T_Expr) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   procedure Handle_Fold
     (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   procedure Handle_Filter
     (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   procedure Handle_New (Create_Tree : T_Create_Tree) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   procedure Handle_All
     (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   procedure Handle_Selector
     (Expr : T_Expr; Suffix : in out T_Expr_Vectors.Vector) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   procedure Handle_Arithmetic_Operator (Expr : T_Expr) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   procedure Handle_Global_Identifier (Name : Text_Type);

   procedure Compute_Selector_Suffix (Suffix : T_Expr_Vectors.Vector) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   -------------------------
   -- Evaluate_Expression --
   -------------------------

   procedure Evaluate_Expression (Expr : T_Expr) is
      --  Some expression need to run the outer action. For example:
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
      Run_Outer_Action : Boolean := True;

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
               Top_Context.Name_Captured := To_Unbounded_Text (Captured_Name);

               if Get_Local_Symbol (Captured_Name) /= null then
                  Previous_Value := Get_Local_Symbol (Captured_Name);
               end if;

               Evaluate_Expression (Expr.Match_Capture_Expr);

               if Top_Object /= Match_False then
                  Include_Symbol (Captured_Name, Top_Object);
               else
                  --  For early reference, that name may have already been
                  --  captured. If we eneded up not having a match, it needs
                  --  to be removed, or replaced by the previous value.

                  if Previous_Value /= null then
                     Include_Symbol (Captured_Name, Previous_Value);
                  elsif Get_Local_Symbol (Captured_Name) /= null then
                     Top_Frame.Symbols.Delete (Captured_Name);
                  end if;
               end if;

               Pop_Frame_Context;
               Run_Outer_Action := False;
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
            Run_Outer_Action := False;

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

                     Run_Outer_Action := False;

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

                     Run_Outer_Action := False;

                  when Template_Operator_Amp =>
                     declare
                        Slice : Buffer_Slice := Get_Empty_Slice;
                     begin
                        Push_Frame_Context_No_Outer;
                        Push_Buffer_Cursor;
                        Slice.Last := Evaluate_Expression
                          (Expr.Binary_Left).Write_String.Last;
                        Slice.Last := Evaluate_Expression
                          (Expr.Binary_Right).Write_String.Last;
                        Push_Object
                          (W_Object'
                             (new W_String_Type'
                                  (Value  => To_Unbounded_Text
                                       (Buffer.Str
                                          (Slice.First.Offset
                                           .. Slice.Last.Offset)))));

                        Pop_Buffer_Cursor;
                        Pop_Frame_Context;
                     end;
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
            declare
               Slice : Buffer_Slice;
            begin
               Push_Frame_Context_No_Outer;
               Push_Buffer_Cursor;
               Slice := Evaluate_String (Expr);

               if Expr.Str_Kind = String_Regexp then
                  --  If we wanted a regexp, pop the object on the stack and
                  --  replace is with a regexp wrapper.
                  Push_Object
                    (W_Object'
                       (new W_Regexp_Type'
                            (Value => To_Unbounded_Text
                                 (Buffer.Str
                                    (Slice.First.Offset
                                     .. Slice.Last.Offset)))));
               else
                  Push_Object
                    (W_Object'
                       (new W_String_Type'
                            (Value => To_Unbounded_Text
                                 (Buffer.Str
                                    (Slice.First.Offset
                                     .. Slice.Last.Offset)))));
               end if;

               Pop_Buffer_Cursor;
               Pop_Frame_Context;
            end;

         when Template_Call_Expr =>
            Push_Frame_Context_No_Pick;
            Handle_Call (Expr);
            Pop_Frame_Context;

            --  Prepare the matching context for the resulting value. As we're
            --  on a call match, we can change the context without pushing /
            --  popping (there's nothing else).

            if Top_Context.Match_Mode = Match_Ref_Default then
               Top_Context.Match_Mode := Match_Call_Default;
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
            if Top_Context.Allocate_Callback /= null then
               Handle_New (Expr.Tree);
            else
               Push_Match_False;
            end if;

            Push_Frame_Context;
            Top_Context.Match_Mode := Match_Has;
            Do_Pop_Frame_Context   := True;

         when Template_At_Ref =>
            if Top_Context.Left_Value = null then
               Error ("no left value available in this context");
            else
               Push_Object (Top_Frame.Top_Context.Left_Value);
            end if;

         when Template_Qualified_Match =>
            --  We are on an expression like has (something) or is (something).
            --  Specify the kind of match we need to make, which will override
            --  the default.

            if Top_Context.Match_Mode = Match_None then
               Error
                 ("qualified match operators only available in match context");
            end if;

            Push_Frame_Context;
            Top_Context.Outer_Expr_Action := Action_Match;

            if Expr.Node.As_Qualified_Match.F_Op = Template_Operator_Is then
               Top_Context.Match_Mode := Match_Is;
            else
               Top_Context.Match_Mode := Match_Has;
            end if;

            Evaluate_Expression (Expr.Qualified_Match_Expr);

            Pop_Frame_Context;

            Run_Outer_Action := False;

         when Template_Match_Expr =>
            Push_Frame_Context_No_Pick;

            if Evaluate_Match (Expr.Match_Match_Expr) then
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

      if Run_Outer_Action then
         Execute_Expr_Outer_Action;
      end if;

      if Do_Pop_Frame_Context then
         Pop_Frame_Context;
      end if;

      Pop_Error_Location;
   end Evaluate_Expression;

   -------------------------
   -- Evaluate_Expression --
   -------------------------

   function Evaluate_Expression (Expr : T_Expr) return W_Object is
   begin
      Evaluate_Expression (Expr);

      return Pop_Object;
   end Evaluate_Expression;

   ----------------------------
   -- Push_Global_Identifier --
   ----------------------------

   function Push_Global_Identifier (Name : Text_Type) return Boolean is
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
      elsif Name = "buffer_line" then
         Push_Object
           (W_Object'(new W_Integer_Type'(Value => Buffer.Cursor.Line)));
         return True;
      elsif Name = "buffer_col" then
         Push_Object
           (W_Object'(new W_Integer_Type'(Value => Buffer.Cursor.Column)));
         return True;
      elsif Name = "buffer_max_col" then
         Push_Intrinsic_Function (null, Call_Max_Col'Access);
         return True;
      end if;

      --  Check in the dynamic symols in the frame

      Tentative_Symbol := Get_Local_Symbol (Name);

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

   -------------------------------
   -- Execute_Expr_Outer_Action --
   -------------------------------

   procedure Execute_Expr_Outer_Action is
   begin
      case Top_Context.Outer_Expr_Action is
         when Action_Match =>
            if Top_Context.Match_Mode not in Match_None | Match_Has then
               --  If we're matching, and we're not forcing the "has" mode,
               --  then check that the initial object we had on the stack
               --  matches the new one.

               if not Top_Context.Outer_Object.Match_With_Top_Object then
                  Pop_Object;
                  Push_Match_False;
               end if;
            end if;

         when Action_Post_Pick_Exec =>
            if Top_Context.Function_Result_Callback /= null
              and then Top_Context.Current_Command.Command_Sequence = null
            then
               --  We are on the pick of a function, not on a command. The
               --  Function result callback has been set and need to be called
               --  to process the result.

               Top_Context.Function_Result_Callback.all;
            else
               --  We are on the expression of the pick clause of a regular
               --  command. Execute the Post_Pick section.

               Handle_Command_Post_Pick (Top_Context.Current_Command);
            end if;

         when Action_None =>
            null;

      end case;
   end Execute_Expr_Outer_Action;

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
      Name          : Text_Type := Node.Text;
      Prefix_Entity : W_Object;
   begin
      --  We're resolving a reference to an entity

      if Top_Context.Is_Root_Selection then
         --  If we're on the implicit entity, then first check if there's
         --  some more global identifier overriding it.

         if Push_Global_Identifier (Node.Text) then

            return;
         end if;

         --  Retreive the entity from implicit It. If Implicit new exist,
         --  we need to also attempt at retreiving its value. We'll return
         --  either the entity coming from one of the two, or raise an error
         --  if both contain such name.

         if Get_Implicit_It.Push_Value (Name) then
            return;
         end if;

         if Top_Context.Match_Mode /= Match_None then
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
            if Top_Context.Match_Mode /= Match_None then
               Push_Match_False;
               return;
            else
               Error ("'" & Name & "' component not found");
            end if;
         end if;
      end if;
   end Handle_Identifier;

   --------------------------
   -- Handle_Template_Call --
   --------------------------

   function Handle_Template_Call
     (Instance : W_Object; Args : T_Arg_Vectors.Vector)
      return Visit_Action
   is
      --  If not already evaluated, parameter evaluation for templates goes as
      --  follows: (1) actual expressions for parameters on a template call are
      --  stored
      --      in the current frame.
      --  (2) when variables are encountered in the following sequences, they
      --      are first evaluated, then
      --      (a) if there's an expression available the ordered expression
      --          list, it will be evaluated
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
      --   Wrapping T this way would evaluate V as "something and something
      --   else".
      --
      --   If the template has already been evaluated, then we only update its
      --   variables.

      procedure Store_Parameter
        (Name : Text_Type; Position : Integer; Value : T_Expr);

      procedure Update_Parameter
        (Name : Text_Type; Position : Integer; Value : T_Expr);

      procedure Handle_Template_Call_Recursive (A_Template : T_Template);

      A_Template_Instance : W_Template_Instance :=
        W_Template_Instance (Instance);

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
         Top_Context.Left_Value := Ref.Value;
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
            Handle_Command_In_Current_Frame (A_Template.Program);
         end if;
      end Handle_Template_Call_Recursive;

      Visit_Result : aliased Visit_Action := Unknown;
   begin
      if A_Template_Instance.Is_Evaluated then
         Process_Parameters (Args, Update_Parameter'Access);

         return Into;
      else
         A_Template_Instance.Is_Evaluated := True;

         Process_Parameters (Args, Store_Parameter'Access);

         Push_Frame (A_Template_Instance.Defining_Entity);
         Push_Implicit_It (A_Template_Instance);
         Top_Context.Visit_Decision := Visit_Result'Unchecked_Access;
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

      if Top_Context.Match_Mode = Match_Ref_Default then
         Top_Context.Match_Mode := Match_Call_Default;
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

      Top_Context.Outer_Expr_Action := Action_Match;

      Called := Evaluate_Expression (Expr.Called).Dereference;

      Pop_Frame_Context;

      --  If the called identifier didn't match, we either just push a match
      --  false if we're in a matching section, or raise an error. Otherwise,
      --  execute the call on the retreived function

      if Called = Match_False then
         if Top_Context.Match_Mode /= Match_None then
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

   procedure Compute_Selector_Suffix (Suffix : T_Expr_Vectors.Vector) is
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
      Top_Context.Name_Captured  := To_Unbounded_Text ("");
      Top_Context.Yield_Callback := null;

      for I in Suffix.First_Index .. Suffix.Last_Index - 1 loop
         Suffix_Expression := Suffix.Element (I);

         Evaluate_Expression (Suffix_Expression);

         if Has_Prev then
            Pop_Underneath_Top;
         end if;

         Has_Prev                                := True;
         Top_Context.Is_Root_Selection := False;
      end loop;

      Pop_Frame_Context;
      Push_Frame_Context;
      Top_Context.Is_Root_Selection := False;

      --  Run the terminal separately. In particular in the case of:
      --     x.y.z.child().all()
      --  while x.y.z needs to be called outside of the yield context, child
      --  () need to be called with the frame context set by all which set in
      --  particular yiekd context to true.

      Evaluate_Expression (Terminal);

      if Has_Prev then
         Pop_Underneath_Top;
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
         Top_Context.Is_Root_Selection := True;
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

      procedure Yield_Callback;

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
            Top_Context.Left_Value := Current_Expression;
            Evaluate_Expression (Fold_Expr.Separator);
            Current_Expression := Pop_Object;
            Pop_Frame_Context;
         end if;

         Push_Frame_Context;
         Top_Context.Left_Value := Current_Expression;
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
      if Top_Context.Name_Captured /= "" then
         Include_Symbol
           (To_Text (Top_Context.Name_Captured), Current_Expression);
      end if;

      Pop_Object;
      Pop_Frame_Context;

      Push_Frame_Context;
      Top_Context.Yield_Callback    := Yield_Callback'Unrestricted_Access;
      Top_Context.Match_Mode        := Match_None;
      Top_Context.Outer_Expr_Action := Action_None;
      Top_Context.Is_Root_Selection := True;

      Evaluate_Expression (Selector.Selector_Left);

      --  The prefix will have pushed its own result which needs to be
      --  disregarded at this stage. Push the fold result instead.

      Pop_Object;
      Push_Object (Current_Expression);

      Pop_Frame_Context;

      if Suffix.Length > 0 then
         Compute_Selector_Suffix (Suffix);
         Pop_Underneath_Top;
      elsif Top_Context.Outer_Expr_Action /= Action_None then
         Push_Frame_Context;

         if Top_Context.Match_Mode = Match_Ref_Default then
            Top_Context.Match_Mode := Match_Call_Default;
         end if;

         Execute_Expr_Outer_Action;

         Pop_Frame_Context;
      end if;
   end Handle_Fold;

   -------------------
   -- Handle_Filter --
   -------------------

   procedure Handle_Filter (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector)
   is

      procedure Generator (Expr : T_Expr);

      procedure Object_Generator
        (Node : access W_Object_Type'Class; Expr : T_Expr);

      Filtered_Expr   : T_Expr := Selector.Selector_Right.Filter_Expr;
      Prefix_Function : T_Expr;

      Object_Mode : Boolean;

      Match_Mode : Match_Kind;

      ---------------
      -- Generator --
      ---------------

      procedure Generator (Expr : T_Expr) is

         procedure Yield_Callback;

         Original_Yield : Yield_Callback_Type := Top_Context.Yield_Callback;

         --------------------
         -- Yield_Callback --
         --------------------

         procedure Yield_Callback is
         begin
            Push_Implicit_It (Top_Object);
            Push_Match_Result (Expr);
            Pop_Underneath_Top;

            if Top_Object /= Match_False then
               if Original_Yield /= null then
                  --  We are generating values, calling the original generator.

                  Call_Yield (Original_Yield);
               else
                  --  We're just looking for the first matching value,
                  --  interrupt the current iteration

                  Parent_Frame.Interrupt_Program := True;
               end if;
            end if;
         end Yield_Callback;

      begin
         Push_Frame_Context_No_Match;
         Top_Context.Match_Mode := Match_Mode;
         Top_Context.Yield_Callback := Yield_Callback'Unrestricted_Access;

         --  We may be called from an anchored context. However, this anchor
         --  should not be passed to the prefix, to which we're just getting
         --  values one by one.
         Top_Context.Regexpr_Anchored := False;

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
      if Top_Context.Match_Mode /= Match_None then
         --  If we enter the filter in any match mode, then we're running a
         --  match operation. The Match_Has filter will be tolerant to prefixes
         --  that don't exist and stack a Match_False instead of an error in
         --  these cases.

         Match_Mode := Match_Has;
      end if;

      Push_Frame_Context_No_Match;

      Top_Context.Match_Mode := Match_Mode;
      Top_Context.Is_Root_Selection := True;

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
         Pop_Underneath_Top;
         Object_Mode := True;
      else
         Object_Mode := False;
      end if;

      Evaluate_Generator_Regexp
        (Root      => Top_Object,
         Generator => Generator'Unrestricted_Access,
         Expr      => Filtered_Expr);

      Pop_Underneath_Top;
      Pop_Frame_Context;

      if Suffix.Length > 0 then
         Compute_Selector_Suffix (Suffix);
         Pop_Underneath_Top;
      elsif Top_Context.Outer_Expr_Action /= Action_None then
         Push_Frame_Context;

         if Top_Context.Match_Mode = Match_Ref_Default then
            Top_Context.Match_Mode := Match_Call_Default;
         end if;

         Execute_Expr_Outer_Action;

         Pop_Frame_Context;
      end if;
   end Handle_Filter;

   ----------------
   -- Handle_All --
   ----------------

   procedure Handle_All (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) is

      procedure Yield_Callback;

      Initial_Context : Frame_Context := Top_Context;

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

         Visit_Decision := Top_Context.Visit_Decision;

         --  Restore the context at this point of the call. This is important
         --  in particular if there was an expansion happening there, e.g.
         --  a.all().b.all().
         Push_Frame_Context (Initial_Context.all);

         --  We still however need to keep control to the same visit iteration
         Top_Context.Visit_Decision := Visit_Decision;

         if Suffix.Length > 0 then
            --  If there's a suffix, then compute it to get the value of the
            --  expansion. The outer action has to be a match check here.
            --  If it's a post pick execute, this is only to be called on
            --  the returning value.

            Push_Frame_Context;
            Top_Context.Outer_Expr_Action := Action_Match;
            Compute_Selector_Suffix (Suffix);
            Pop_Frame_Context;
         else
            --  Otherwise, the value is the just the last stacked object.
            --  Dereference it to remove the potential "It" attribute
            Push_Object (Top_Object.Dereference);
         end if;

         --  .all () may itself be in an expression such as .all().fold(). In
         --  this case an expand action is set and needs to be executed.
         Call_Yield (Initial_Context.Yield_Callback);

         if Initial_Context.Outer_Expr_Action /= Action_None then
            --  Execute the outer action once per run of the suffix, which may
            --  be a Outer_Expression_Pick call.

            Execute_Expr_Outer_Action;
         end if;

         Pop_Frame_Context;
      end Yield_Callback;

   begin
      Push_Frame_Context_No_Match;

      Top_Context.Yield_Callback := Yield_Callback'Unrestricted_Access;

      Evaluate_Expression (Selector.Selector_Left);

      Pop_Frame_Context;
   end Handle_All;

   ----------------
   -- Handle_New --
   ----------------

   procedure Handle_New (Create_Tree : T_Create_Tree) is

      function Handle_Create_Template
        (New_Tree : T_Create_Tree; Parent : W_Template_Instance)
         return W_Template_Instance;

      function Handle_Create_Tree
        (A_Tree : T_Create_Tree; Parent : W_Template_Instance)
         return W_Template_Instance;

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
             (T_Template (New_Tree.Call.Reference), null, True);

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

            Top_Context.Allocate_Callback.all (New_Node);
         else
            Add_Wrapping_Child (Parent, New_Node);
         end if;

         Dummy_Action := Handle_Template_Call
           (W_Object (New_Node), New_Tree.Call.Args);
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
      Top_Context.Match_Mode := Match_Has;

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

end Wrapping.Runtime.Expressions;
