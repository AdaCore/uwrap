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

with Ada.Containers;                  use Ada.Containers;
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

   procedure Handle_Identifier (Name : Text_Type) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Stacks the W_Object that corresponds to the name in parameter at the
   --  current position in the program and frame.

   procedure Handle_Call (Expr : T_Expr) with
     Pre  => Expr.Kind = Template_Call_Expr,
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Stacks the result of the call.

   procedure Handle_Fold
     (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) with
     Pre  => Selector.Selector_Right.Kind = Template_Fold_Expr,
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Executes the fold expression on its prefix and computes the afterwards
   --  if any.

   procedure Handle_Filter
     (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) with
     Pre  => Selector.Selector_Right.Kind = Template_Filter_Expr,
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Executes the filter expression on its prefix and computes the afterwards
   --  if any.

   procedure Handle_New (Create_Tree : T_Create_Tree) with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Creates the nodes described by this Create_Tree object.

   procedure Handle_All
     (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) with
     Pre  => Selector.Selector_Right.Kind = Template_All_Expr,
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Will call the suffix for all values generated from the suffix, or will
   --  also execute the post pick action if any.

   procedure Handle_Selector
     (Expr : T_Expr; Suffix : in out T_Expr_Vectors.Vector) with
     Pre  => Expr.Kind = Template_Selector,
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Will process the selector in parameter and push the selectd value on
   --  the stack.

   procedure Handle_Arithmetic_Operator (Expr : T_Expr) with
     Pre  => Expr.Kind = Template_Binary_Expr,
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Handle the aritmetic operation in parameter on the last two objects
   --  on the stack.

   procedure Compute_Selector_Chain (Chain : T_Expr_Vectors.Vector) with
     Pre  => Chain.Length > 0,
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Selectors work by stacking entities in a Suffix variable as they're
   --  going from last in the list to first. Once hitting first, they have
   --  a vector of expression that represent each part. This function will
   --  then process each part from first to last.

   -------------------------
   -- Evaluate_Expression --
   -------------------------

   procedure Evaluate_Expression (Expr : T_Expr) is
      Run_Outer_Action : Boolean := True;
      --  Some expression need to run the outer action. For example:
      --    match a
      --  a needs to match with It. Other do not. For example in :
      --     match a or b
      --  we need to match individually a and b, but not the expression (a or
      --  b)
      --  TODO: we should really disconnect outer match and pick, as we don't
      --  want to follow the same semantic for pick (in a or b, we only want
      --  to pick the result. At this point, use Top_Object.Match_Mode instead
      --  of this flag.
   begin
      Push_Frame_Context;
      Push_Error_Location (Expr.Node);

      case Expr.Kind is
         when Template_Match_Capture =>
            declare
               Captured_Name : constant Text_Type :=
                 Expr.Node.As_Match_Capture.F_Captured.Text;
               Previous_Value : W_Object;
            begin
               --  This expression captures the result of the underlying
               --  expression and lets its value pass through.

               Push_Frame_Context;

               --  We need to save the name capture as some expressions need
               --  to maintain its value while being computed. E.g. in:
               --     c: child ()
               --  the value of c as currently computed is available in child.

               Top_Context.Name_Captured := To_Unbounded_Text (Captured_Name);

               --  Save any previous name capture for restoration, and store
               --  the new one.

               if Get_Local_Symbol (Captured_Name) /= null then
                  Previous_Value := Get_Local_Symbol (Captured_Name);
               end if;

               --  Evaluate the capture expression and push the result on the
               --  stack. In all cases, the result of this sub-expression will
               --  be the result of the result of the capture expression.

               Evaluate_Expression (Expr.Match_Capture_Expr);

               if Top_Object /= Match_False then
                  --  If we got something, stores the result in the symbol

                  Include_Symbol (Captured_Name, Top_Object);
               else
                  --  If we didn't get anything, restore the previous value if
                  --  any, or delete the symbol from the frame in case it has
                  --  been pushed as an early reference

                  if Previous_Value /= null then
                     Include_Symbol (Captured_Name, Previous_Value);
                  elsif Get_Local_Symbol (Captured_Name) /= null then
                     Top_Frame.Symbols.Delete (Captured_Name);
                  end if;
               end if;

               Pop_Frame_Context;

               --  The outer action is performed by the expression below a
               --  match capture, not by the capture itself, as the expression
               --  may itself make different decisions as to how to run that
               --  outer action, e.g. in:
               --     match x: (a or b)
               --  a and b expression need to run the outer action to decide
               --  wether they match or not.

               Run_Outer_Action := False;
            end;

         when Template_Selector =>
            declare
               Suffix : T_Expr_Vectors.Vector;
            begin
               --  At the begining of a selector analysis, sets the suffix as
               --  an empty vector. Handle_Selector will then be responsible to
               --  call itself recursively and fill this suffix when needed,
               --  before eventually executing it. See implementation of
               --  Handle_Selector for more details

               Handle_Selector (Expr, Suffix);
            end;

            --  Never match the result of a selection. Matching happened in
            --  Handle_Selector, when evaluating the right operand. At this
            --  stage, we may also not be in the right match mode anymore
            --  (e.g we don't know if we match a reference or a call result).
            --  This is also how we avoid dupliquated picked values, e.g. in
            --     pick x.child ()
            --  or
            --     pick x.child ().all ()
            --  The pick action is driven by the selected expression (e.g.
            --  respectively child () and all ()), not by the selector.

            Run_Outer_Action := False;

         when Template_Binary_Expr =>
            declare
               Left, Right : W_Object;
            begin
               case Expr.Node.As_Binary_Expr.F_Op.Kind is
                  --  The convention for "and" and "or" binary operators is
                  --  to push to the stack the last object that matched,
                  --  otherwise false. This allows to capture that object later
                  --  on, which can be useful for example if that object is a
                  --  newly allocated one, e.g in
                  --     a or new (some_name ())

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

                     --  Outer actions are resolved by the operands. E.g. in:
                     --     it (a or b)
                     --  a and b are matched agains it one after the other,
                     --  but the result doesn't need to be.

                     Run_Outer_Action := False;

                  when Template_Operator_Or =>
                     --  See And implementation

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
                        Slice       : Buffer_Slice;
                     begin
                        --  Concatenation evaluates both object, then write
                        --  their string contents to the buffer one after the
                        --  other - the resulting slice will effectively be
                        --  the concatenation of the two results.

                        Push_Frame_Context_No_Outer;

                        Push_Buffer_Cursor;

                        Slice := Evaluate_Expression
                          (Expr.Binary_Left).Write_String;
                        Slice.Last := Evaluate_Expression
                          (Expr.Binary_Right).Write_String.Last;

                        Push_Object
                          (To_W_String
                             (Buffer.Str
                                  (Slice.First.Offset .. Slice.Last.Offset)));

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
               Right : constant W_Object :=
                 Evaluate_Expression (Expr.Unary_Right).Dereference;
            begin
               if Expr.Node.As_Unary_Expr.F_Op.Kind = Template_Operator_Not
               then
                  if Right = Match_False then
                     --  In the case of a "not" operator, stack the top object,
                     --  so that something like:
                     --     it (x: not something);
                     --  will match the value in x that correspond to the
                     --  entity that validated the expression.

                     Push_Match_True (Top_Object.Dereference);
                  else
                     Push_Match_False;
                  end if;
               end if;
            end;

            Pop_Frame_Context;

         when Template_Literal =>
            if Expr.Node.Text = "true" then
               --  A "true" value automatically takes the value of the top
               --  object so that the value of an expression can easily be
               --  captured, as in:
               --     a.b.child (x: true).c

               Push_Match_True (Top_Object.Dereference);
            elsif Expr.Node.Text = "false" then
               Push_Match_False;
            else
               Error ("unkown literal '" & Expr.Node.Text & "'");
            end if;

         when Template_Token_Identifier | Template_Identifier =>
            Push_Frame_Context_No_Outer;
            Handle_Identifier (Expr.Node.Text);
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
                  --  Otherwise just push the string

                  Push_Object
                    (To_W_String
                       (Buffer.Str
                            (Slice.First.Offset .. Slice.Last.Offset)));
               end if;

               Pop_Buffer_Cursor;
               Pop_Frame_Context;
            end;

         when Template_Call_Expr =>
            Push_Frame_Context_No_Pick;
            Handle_Call (Expr);
            Pop_Frame_Context;

            --  Prepare the matching context for matching the resulting value
            --  If the match mode is the default ref, switch to default call. O
            --  bjects usually don't compare their result with a call but just
            --  check the fact that a call work, e.g. in:
            --     it (child (x))
            --  it will check that there's a child (x) as opposed to checking
            --  that this child is equal to it.

            if Top_Context.Match_Mode = Match_Ref_Default then
               Top_Context.Match_Mode := Match_Call_Default;
            end if;

         when Template_Defer_Expr =>
            declare
               Deferred_Expr : constant W_Deferred_Expr :=
                 new W_Deferred_Expr_Type;
            begin
               --  If we're on a defered expression, just capture the
               --  environment and push the expression.

               Capture_Deferred_Environment (Deferred_Expr, Expr);
               Push_Object (Deferred_Expr);
            end;

         when Template_New_Expr =>
            --  Expression that contain allocators are executed twice in
            --  certain situations, for example when doing a tree traversal,
            --  once to check if they can match without the need of the
            --  allocator (e.g. if allocation fail) and one with the allocator
            --  allowed. Only handle new in that second stage, push false
            --  otherwise.

            if Top_Context.Allow_Allocate then
               Handle_New (Expr.New_Tree);
            else
               Push_Match_False;
            end if;

            Top_Context.Match_Mode := Match_Has;

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

            --  We already performed a match inside the qualifier. Don't
            --  perform another one again here.

            Top_Context.Match_Mode := Match_None;

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

      Pop_Error_Location;
      Pop_Frame_Context;
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

      --  If we haven't found an intrinsic, check the dynamic symbols in the
      --  frame. Warn in case of ambiguities

      Tentative_Symbol := Get_Local_Symbol (Name);

      A_Module := Get_Module (Top_Frame.all);

      --  Check if the current module is the name we're looking for

      if To_Text (A_Module.Name) = Name then
         if Tentative_Symbol /= null then
            Error ("symbol name hiding module name");
         end if;

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

      --  If we haven't found a symbol yet, then there's no global identifier,
      --  return false to allow other search processing. Otherwise, push
      --  the initial symbol and return true to signal success.

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

   -----------------------
   -- Handle_Identifier --
   -----------------------

   procedure Handle_Identifier (Name : Text_Type) is
      Prefix_Entity : W_Object;
   begin
      --  We're resolving a reference to an entity

      if Top_Context.Is_Root_Selection then
         --  If we're at the start of a selection (or not on a selection), we
         --  may be refering to a global identifier.

         if Push_Global_Identifier (Name) then

            return;
         end if;

         --  If we're not refering to a global identifier, we may instead
         --  be refering to a field or method of the implicit it.

         if Get_Implicit_It.Push_Value (Name) then
            return;
         end if;

         --  If neither is found, either we're in a match process, so we just
         --  push match false, or we're in a situation that does expect a value
         --  and we need to signal an error.

         if Top_Context.Match_Mode /= Match_None then
            Push_Match_False;
            return;
         else
            Error ("'" & Name & "' not found");
         end if;
      else
         --  We're on a selection. Retreives the prefix and try to resolve
         --  the suffix.

         Prefix_Entity := Top_Object.Dereference;

         if Prefix_Entity = Match_False then
            Error ("prefix not found");
         end if;

         if Prefix_Entity.Push_Value (Name) then
            --  We found a component of the entity and it has been pushed

            return;
         else
            --  If we can't find the suffix, either we're on a match case then
            --  just stack match false, or we were expecting a value and thus
            --  need to log an error.

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

   procedure Handle_Template_Call
     (Instance : W_Object; Args : T_Arg_Vectors.Vector)
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
      --  Callback used for the initial creation of the template, storing
      --  the parameter to the list to be evaluated later on when processing
      --  the template.

      procedure Update_Parameter
        (Name : Text_Type; Position : Integer; Value : T_Expr);
      --  Callback used when updating an already created template, updating
      --  values already created.

      procedure Handle_Template_Call_Recursive (A_Template : T_Template);
      --  Recursively evaluate parents of a template.

      A_Template_Instance : constant W_Template_Instance :=
        W_Template_Instance (Instance);

      ---------------------
      -- Store_Parameter --
      ---------------------

      procedure Store_Parameter
        (Name : Text_Type; Position : Integer; Value : T_Expr)
      is
         pragma Unreferenced (Position);
      begin
         --  Stores the parameter expression in the top frame, for evaluation
         --  later on when going through the template command.

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
         --  Checks that the parameter provided does exist in the template

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

         --  Set the left value, which is the value contained by the reference
         --  used for that variable. Then update the referenced value to the
         --  result of the expression.

         Push_Frame_Context;
         Top_Context.Left_Value := Ref.Value;
         Ref.Value := Evaluate_Expression (Value);

         Pop_Frame_Context;
      end Update_Parameter;

      ------------------------------------
      -- Handle_Template_Call_Recursive --
      ------------------------------------

      procedure Handle_Template_Call_Recursive (A_Template : T_Template) is
      begin
         --  First evaluates the parent template, then itself.

         if A_Template.Extends /= null then
            Handle_Template_Call_Recursive (A_Template.Extends);
         end if;

         if A_Template.Program /= null then
            Handle_Command_In_Current_Frame (A_Template.Program);
         end if;
      end Handle_Template_Call_Recursive;

   begin
      if A_Template_Instance.Is_Evaluated then
         --  If the tempalte has already been evaluated, then update the
         --  parameters

         Process_Parameters (Args, Update_Parameter'Access);
      else
         --  If this template has not been evaluated yet, set the flag.

         A_Template_Instance.Is_Evaluated := True;

         --  First, store all arguments in the top frame. They will be
         --  evaluated later, as the template variables are created and
         --  initialized by the Handle_Command_Sequence call.

         Process_Parameters (Args, Store_Parameter'Access);

         if A_Template_Instance.Defining_Entity.Full_Name =
           "standard.root"
         then
            --  standard.root is the main program. It operates on the current
            --  "it" at the point of the call, but does not lead to the
            --  evaluation of an actual template call. This is a shortcut
            --  allowing to directy the main processing, and should only be
            --  used through a walk.
            --  TODO: add a way to verify that this is always called through
            --  walk

            Apply_Wrapping_Program
              (W_Node (Get_Implicit_It),
               Wrapping.Semantic.Analysis.Root);
         else
            --  Template evaluation is done in its own frame, where the current
            --  "it" object is switched to this new instance.

            Push_Frame (A_Template_Instance.Defining_Entity);
            Push_Implicit_It (A_Template_Instance);
            Top_Frame.Current_Template := W_Object (A_Template_Instance);

            --  This call will evaluate the template and its parents. It will
            --  also create variables and evaluate parameters

            Handle_Template_Call_Recursive
              (T_Template (A_Template_Instance.Defining_Entity));

            --  Check that all parameters have been used by the template call.
            --  If not, the actual parameters do not correspond to the template
            --  structure, raise an error.

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
      --  the default call mode. If it were Match_Has or Match_Is, that would
      --  mean that the user explicitely decided on the mode through an is ()
      --  or has () qualifier, which we'd want to keep.

      if Top_Context.Match_Mode = Match_Ref_Default then
         Top_Context.Match_Mode := Match_Call_Default;
      end if;

      --  Calls can be made on various W_ objects (most of them actually). In
      --  some situations, even if an object is resolved, it should not be
      --  allowed to be called in the current context. For example, one
      --  may resolve a reference to a static entity, as in:
      --     match w_SomeTemplate ()
      --  But if the current "it" object isn't w_SomeTemplate, it should be
      --  rejected. For that reason, we need to set the action to Action_Match.
      --  Note that under Match_Call_Default, most call will be pass through
      --  under match (to the opposite of the above). In particular functions
      --  do not match against the outer object.

      Top_Context.Outer_Expr_Action := Action_Match;

      Called := Evaluate_Expression (Expr.Call_Called).Dereference;

      Pop_Frame_Context;

      if Called = Match_False then
         --  If the called identifier didn't match, we either just push a match
         --  false if we're in a matching section, or raise an error.
         --  Otherwise, execute the call on the retreived function

         if Top_Context.Match_Mode /= Match_None then
            Push_Match_False;
         else
            Error ("call not matching context");
         end if;
      else
         --  Otherwise, call the object that was retreived.

         Called.Push_Call_Result (Expr.Call_Args);

         if not Called.Is_Generator then
            --  If the called subprogram is a generator, then it will have
            --  called the yeild callback upon value generation. Otherwise,
            --  call yield (if any) on the one returned value.

            Call_Yield;
         end if;
      end if;
   end Handle_Call;

   -----------------------------
   -- Compute_Selector_Suffix --
   -----------------------------

   procedure Compute_Selector_Chain (Chain : T_Expr_Vectors.Vector) is
      Has_Prev : Boolean := False;
   begin
      --  The left part of a selector may have calls. In this case, these calls
      --  are unrelated to the value that is possibly being captured. E.g. in:
      --     a: b ().c
      --  b () value is not being captured in a. In order to respect that, the
      --  current captured name is removed when processing the left part of
      --  the selector.
      --  Functions iterating over all generated values such as all () or
      --  fold () cut the sequence so that they are never part of a suffix.
      --  e.g:
      --     a.b.c.fold ()
      --  will create a chain:
      --     a.b.c
      --  Only c can yeild values to fold (). As a result, we're first
      --  computing the n-1 expressions without a yeild callback and will
      --  restore the initial context (and its potential yeild) on the last
      --  element.

      Push_Frame_Context_No_Match;
      Top_Context.Name_Captured  := To_Unbounded_Text ("");
      Top_Context.Yield_Callback := null;

      for I in Chain.First_Index .. Chain.Last_Index - 1 loop
         Evaluate_Expression (Chain.Element (I));

         if Has_Prev then
            Pop_Underneath_Top;
         end if;

         Has_Prev                      := True;
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

      Evaluate_Expression (Chain.Last_Element);

      --  This call is supposed to only stack one element. Remove the second
      --  to last if there was already a value stacked prior to the call to
      --  Evaluate_Expression.

      if Has_Prev then
         Pop_Underneath_Top;
      end if;

      Pop_Frame_Context;
   end Compute_Selector_Chain;

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
         --  We're on a selector with a previous selector, e.g.:
         --     a.b.c
         --  Add c to the suffix, and handle the a.b selector.

         Suffix.Prepend (Expr.Selector_Right);
         Handle_Selector (Expr.Selector_Left, Suffix);
      else
         --  We're on a selector with only two elements. Add them to the
         --  suffix, which becomes at this stage the whole change. Then compute
         --  the selector chain from start.

         Suffix.Prepend (Expr.Selector_Right);
         Suffix.Prepend (Expr.Selector_Left);

         Compute_Selector_Chain (Suffix);
      end if;
   end Handle_Selector;

   -----------------
   -- Handle_Fold --
   -----------------

   procedure Handle_Fold (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector) is

      procedure Yield_Callback;
      --  Called when a value from the prefix has been generated and computes
      --  the result.

      Fold_Expr : constant T_Expr := Selector.Selector_Right;

      Is_First : Boolean := True;

      Current_Expression : W_Object;

      --------------------
      -- Yield_Callback --
      --------------------

      procedure Yield_Callback is
      begin
         if Is_First then
            --  If this is the first value generated, just switch the first
            --  switch

            Is_First := False;
         elsif Fold_Expr.Fold_Separator /= null then
            --  Otherwise, if there's an expression to execute to separate
            --  two values, execute it.

            Push_Frame_Context_Parameter;
            Top_Context.Left_Value := Current_Expression;
            Current_Expression :=
              Evaluate_Expression (Fold_Expr.Fold_Separator);
            Pop_Frame_Context;
         end if;

         --  Execute the combine expression in all cases.

         Push_Frame_Context;
         Top_Context.Left_Value := Current_Expression;
         Evaluate_Expression (Fold_Expr.Fold_Combine);
         Current_Expression := Top_Object;
         Pop_Frame_Context;
      end Yield_Callback;

   begin
      --  Computes the default expression as a parameter

      Push_Frame_Context_Parameter;
      Current_Expression := Evaluate_Expression (Fold_Expr.Fold_Default);

      --  If the name captured is not null, provide its value here. This allows
      --  two equivalent stypes for fold:
      --     x: child ().fold ("", x & something)
      --  or
      --     child ().fold (x: "", x: (x & something))
      --  which is consistent with the overall way capture works.

      if not (Top_Context.Name_Captured = "") then
         Include_Symbol
           (To_Text (Top_Context.Name_Captured), Current_Expression);
      end if;

      Pop_Frame_Context;

      --  Prepare the context for the prefix. The yield callback will be
      --  resonsible to gather the values generated and to build the result.
      --  We're not matching anything in the prefix, and aren't doing any
      --  action with the outer expression. When entering in the selector,
      --  we're as if we were a root selection, e.g. in
      --     a.b.c.fold ()
      --  a.b.c is a root selection. Evaluate expression will be able to
      --  then split the above, or detect that we're actually on a unique
      --  selector, e.g. in:
      --     a.fold ()
      --  and will indeed treat a as a root selection.

      Push_Frame_Context;
      Top_Context.Yield_Callback    := Yield_Callback'Unrestricted_Access;
      Top_Context.Match_Mode        := Match_None;
      Top_Context.Outer_Expr_Action := Action_None;
      Top_Context.Is_Root_Selection := True;

      Evaluate_Expression (Selector.Selector_Left);

      --  Evaluate_Expression will have pushed its own result which needs to be
      --  disregarded at this stage. Push the fold result instead.

      Pop_Object;
      Push_Object (Current_Expression);

      Pop_Frame_Context;

      if Suffix.Length > 0 then
         --  There are more element to compute after fold, e.g. in
         --     a.b.fold ().c
         --  Compute the suffix that was previously set, and remove the result
         --  of fold from the stack after that.

         Compute_Selector_Chain (Suffix);
         Pop_Underneath_Top;
      elsif Top_Context.Outer_Expr_Action /= Action_None then
         --  fold () is the last element of this selector chain. Process the
         --  outer action if any, considering fold () as being matched as a
         --  function.

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
      --  Filter will use a regular expression that needs a function to
      --  generate new values. This procedure will generate new values using
      --  the right end of the selector.

      Filtered_Expr   : constant T_Expr := Selector.Selector_Right.Filter_Expr;
      Prefix_Function : T_Expr;

      Object_Mode : Boolean;

      Match_Mode : Match_Kind;

      ---------------
      -- Generator --
      ---------------

      procedure Generator (Expr : T_Expr) is

         procedure Yield_Callback;
         --  This callback will either call the original yeild (in the case
         --  where filter is itself in a situation where all values are
         --  iterated upon, e.g.
         --     a.filter ().all ()
         --  or it will interrupt the current iteration - there's only one
         --  value that we need.

         Original_Yield : constant Yield_Callback_Type :=
           Top_Context.Yield_Callback;

         --------------------
         -- Yield_Callback --
         --------------------

         procedure Yield_Callback is
         begin
            Push_Frame_Context;
            Push_Implicit_It (Top_Object);
            Push_Match_Result (Expr);
            Pop_Underneath_Top;
            Pop_Frame_Context;

            if Top_Object /= Match_False then
               if Original_Yield /= null then
                  --  We are generating values, calling the original generator.

                  Call_Yield (Original_Yield);
               else
                  --  We're just looking for the first matching value,
                  --  interrupt any current function

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

   begin
      if Top_Context.Match_Mode /= Match_None then
         --  The question is when entering something like:
         --     a.filter ()
         --  What to do if a doesn't exit? If we enter the filter in any match
         --  mode, then we're running a match operation, e.g.:
         --     match a.filter ().
         --  The Match_Has filter will be tolerant to prefixes that don't exist
         --  and stack a Match_False instead of an error in these cases.
         --  Otherwise we keep Match_Node and will generate an error if the
         --  prefix can't be found.

         Match_Mode := Match_Has;
      end if;

      Push_Frame_Context_No_Match;

      Top_Context.Match_Mode := Match_Mode;
      Top_Context.Is_Root_Selection := True;

      --  A filter expression is about calling the directly prefixing function
      --  several times to find a matching pattern. First identify the inital
      --  value of "it" and the expression on which the filter is done. In the
      --  case:
      --     X ().filter ()
      --  It is current "it", X is the function. In the case:
      --     A.B.X ().filter ()
      --  X is still the function, but A.B needs to be computed to retreive It.

      case Selector.Selector_Left.Kind is
         when Template_Selector =>
            --  We are in the case:
            --     A.X().filter ()
            --  Computes A to find the first object to generate from.
            --  X () is the prefix function.

            Prefix_Function := Selector.Selector_Left.Selector_Right;
            Evaluate_Expression (Selector.Selector_Left.Selector_Left);

         when others =>
            --  We are in the case:
            --     X().filter ();
            --  X() is the prefix function, the first object to generate from
            --  is "it".

            Prefix_Function := Selector.Selector_Left;
            Push_Object (Top_Object);

      end case;

      --  At this stage, we evaluated the prefix of A.B.X () if any. Now launch
      --  the regexp analysis

      if Prefix_Function.Kind = Template_Identifier then
         --  This is an identifier. Retreives its value. The Generator ()
         --  callback will be able to use it as a generator.

         Evaluate_Expression (Prefix_Function);
         Pop_Underneath_Top;
         Object_Mode := True;
      else
         --  The prefix is not an object, we're going to use Prefix_Function.

         Object_Mode := False;
      end if;

      --  Launch the evaluation

      Evaluate_Generator_Regexp
        (Root      => Top_Object,
         Generator => Generator'Unrestricted_Access,
         Expr      => Filtered_Expr);

      Pop_Underneath_Top;
      Pop_Frame_Context;

      if Suffix.Length > 0 then
         --  If there is more to analyze, e.g. in:
         --     a.b.filter ().c
         --  Computes the suffix.

         Compute_Selector_Chain (Suffix);
         Pop_Underneath_Top;
      elsif Top_Context.Outer_Expr_Action /= Action_None then
         --  Otherwise, run the outer action and finish

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
      --  Callback called by the prefix when generating values

      Initial_Context : constant Frame_Context := Top_Context;

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
         --  a.all().b.all() and to have outer actions set.

         Push_Frame_Context (Initial_Context.all);

         --  We still however need to keep control to the current visit
         --  iteration

         Top_Context.Visit_Decision := Visit_Decision;

         if Suffix.Length > 0 then
            --  If there's a suffix, then compute it to get the value of the
            --  expression.

            Compute_Selector_Chain (Suffix);
         else
            --  Otherwise, the value is the just the last stacked object.
            --  Dereference it to remove the potential "it" attribute
            --  TODO: this is a bit of a kludgy way to handle "it".

            Push_Object (Top_Object.Dereference);

            if Top_Context.Yield_Callback /= null then
               --  We have reached a point that requires generation of a value,
               --  e.g  as .all().fold() (in these cases, the suffix fold ()
               --  would not have passed to all).

               Call_Yield (Top_Context.Yield_Callback);
            elsif Top_Context.Outer_Expr_Action = Action_Post_Pick_Exec then
               --  We're on an expression like pick <expression>.all ().
               --  Execute the pick callback.

               Execute_Expr_Outer_Action;
            end if;
         end if;

         Pop_Frame_Context;
      end Yield_Callback;

   begin
      Push_Frame_Context_No_Match;

      Top_Context.Yield_Callback := Yield_Callback'Unrestricted_Access;

      --  TODO: consider cases where the selector left does not generate
      --  values, e.g.
      --     it.all()
      --  where it is not a generator. We would miss the case where there's
      --  a yield callback to call.

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
      --  Creates a new template instance

      function Handle_Create_Tree
        (A_Tree : T_Create_Tree; Parent : W_Template_Instance)
         return W_Template_Instance;
      --  Recursively calls Handle_Create_Template on all the tree designated
      --  by A_Tree.

      ----------------------------
      -- Handle_Create_Template --
      ----------------------------

      function Handle_Create_Template
        (New_Tree : T_Create_Tree; Parent : W_Template_Instance)
         return W_Template_Instance
      is
         New_Node : W_Template_Instance;
         Captured : constant Text_Type :=
           To_Text (New_Tree.Call.Captured_Name);
      begin
         Push_Error_Location (New_Tree.Node);

         --  Creates a new template instance to be registered for the next
         --  round of iteration, but does not wrap any other tree

         New_Node :=
           Create_Template_Instance (New_Tree.Call.Reference, null, True);

         --  If that node has a captured named ahead of it, store it before
         --  the call so that it can be referenced by the parameters

         if Captured /= "" then
            Include_Symbol (Captured, W_Object (New_Node));
         end if;

         if Parent = null then
            --  If this is the root of the creation, then we need to signal
            --  this outside by calling the allocate callback. This is needed
            --  for constructions like:
            --     child (new (T ())) or child (new ({T(), T()}))
            --  where the root entities need to be connected to the children of
            --  the parent entity.

            --  for the form new (T()), only the first one needs to be
            --  passed above.

            if Top_Context.Allocate_Callback /= null then
               Top_Context.Allocate_Callback.all (New_Node);
            end if;
         else
            --  If we're not on the root, then we know we're creating a child
            --  of the previous layer. Add child with wrapping awareness, in
            --  case the parent is a wrapper.

            Add_Child_With_Wrapping (Parent, New_Node);
         end if;

         --  Once the new template is created and added to the overall
         --  structure, run the call with its parameters.

         Handle_Template_Call (W_Object (New_Node), New_Tree.Call.Args);
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
            --  We're on the case:
            --     new (Some_Template ());
            --  or
            --     new (Some_Template () {A (), B ()});
            --  Create the template for Some_Template, then recursively create
            --  the children if needed, dismissing their values.

            Main_Node := Handle_Create_Template (A_Tree, Parent);

            for C of A_Tree.Subtree loop
               Dummy := Handle_Create_Tree (C, Main_Node);
            end loop;

            return Main_Node;
         else
            --  We are on the case
            --     new ({T(), T()})
            --  In other worlds, we're creating more than one node at the top
            --  level of the tree. There's no node to be created for that
            --  level, recursively call each sub-tree and return the last one
            --  created.

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
      Kind            : constant Template_Node_Kind_Type :=
        Expr.Node.As_Binary_Expr.F_Op.Kind;
   begin
      --  When evaluating operands or arithmetic operators, e.g. in:
      --     1 + 2
      --  there's no outer match on 1 or 2

      Push_Frame_Context_No_Match;

      --  Computes the left and right operands. In case they're returned as
      --  references, dereference the result to access to the actual object,
      --  which is supposed to be an integer.

      Left := Evaluate_Expression (Expr.Binary_Left).Dereference;

      if Left.all not in W_Integer_Type'Class then
         Error
           ("Expected integer for left operand, found " &
            Wide_Wide_Expanded_Name (Left.all'Tag));
      else
         Left_I := W_Integer (Left).Value;
      end if;

      Right := Evaluate_Expression (Expr.Binary_Right).Dereference;

      if Right.all not in W_Integer_Type'Class then
         Error
           ("Expected integer for right operand, found " &
            Wide_Wide_Expanded_Name (Left.all'Tag));
      else
         Right_I := W_Integer (Right).Value;
      end if;

      case Kind is
         when Template_Operator_Plus | Template_Operator_Minus |
           Template_Operator_Multiply | Template_Operator_Divide =>

            --  We're on an arithmetic operator. Push the result.

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

            --  We're on a comparison operator. Compare the two values and
            --  either push the top object (the comparison in the context
            --  of that object matched) or false.

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
      Pop_Frame_Context;
   end Handle_Arithmetic_Operator;

end Wrapping.Runtime.Expressions;
