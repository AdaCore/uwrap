with Ada.Containers; use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Tags; use Ada.Tags;
with Ada.Unchecked_Conversion;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with System; use System;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;
with Libtemplatelang.Common; use Libtemplatelang.Common;
with Wrapping.Semantic.Structure;
with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Wrapping.Runtime.Functions; use Wrapping.Runtime.Functions;
with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;

package body Wrapping.Runtime.Structure is

   Root_Language_Entities : W_Node_Maps.Map;

   function Lt_Wrapper (Left, Right : W_Object) return Boolean is
   begin
      return Left.Lt (Right);
   end Lt_Wrapper;

   function Eq_Wrapper (Left, Right : W_Object) return Boolean is
   begin
      return Left.Eq (Right);
   end Eq_Wrapper;

   function Get_Visible_Symbol (A_Frame: Data_Frame_Type; Name : Text_Type) return W_Object is
   begin
      if Top_Frame.Symbols.Contains (Name) then
         return Top_Frame.Symbols.Element (Name);
      end if;

      return null;
   end Get_Visible_Symbol;

   function Get_Module (A_Frame : Data_Frame_Type) return Semantic.Structure.T_Module is
      use Semantic.Structure;

      Scope : Semantic.Structure.T_Entity := A_Frame.Lexical_Scope;
   begin
      while Scope /= null and then Scope.all not in T_Module_Type'Class loop
         Scope := Scope.Parent;
      end loop;

      return Semantic.Structure.T_Module (Scope);
   end Get_Module;

   function Traverse
     (An_Entity    : access W_Object_Type;
      A_Mode       : Browse_Mode;
      Include_It : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action)
      return Visit_Action
   is
   begin
      Final_Result := null;
      return Into;
   end Traverse;

   function Browse_Entity
     (Browsed : access W_Object_Type'Class;
      Match_Expression : T_Expr;
      Result : out W_Object) return Visit_Action
   is
      Visit_Decision : aliased Visit_Action := Unknown;

      procedure Evaluate_Yield_Function
        with Post => Top_Frame.Data_Stack.Length =
          Top_Frame.Data_Stack.Length'Old
      is
         Yield_Callback : Yield_Callback_Type := Top_Frame.Top_Context.Yield_Callback;
      begin
         --  In certain cases, there's no expression to be evaluated upon
         --  yield. E.g.:
         --    x.all ()
         --  as opposed to:
         --    x.all().something().
         if Yield_Callback = null then
            return;
         end if;

         --  When evaluating a yield callback in a browsing call, we need to
         --  first deactivate yield in the expression itself. We also we need
         --  to remove potential name capture, as it would override the one we
         --  are capturing in this browsing iteration. TODO: quite the opposite if we do fold (i : inti, i: acc);

         Push_Frame_Context_Parameter;
         Top_Frame.Top_Context.Yield_Callback := null;
         Top_Frame.Top_Context.Name_Captured := To_Unbounded_Text ("");
         Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;
         Top_Frame.Top_Context.Visit_Decision := Visit_Decision'Unchecked_Access;

         --  Then evaluate that folding expression

         Push_Implicit_It (Browsed);
         Yield_Callback.all;

         --  The result of the evaluate expression is the result of the
         --  yield callback, as opposed to the matching entity in normal
         --  browsing.
         Result := Pop_Object;
         Pop_Object;

         --  Pop frame context. This will in particular restore the name
         --  catpure, which we're using as the accumulator.
         Pop_Frame_Context;

         --  If there's an name to store the result, store it there.

         if Top_Frame.Top_Context.Name_Captured /= "" then
            Include_Symbol
              (To_Text (Top_Frame.Top_Context.Name_Captured),
               Result);
         end if;
      end Evaluate_Yield_Function;

      Expression_Result : W_Object;

   begin
      Result := null;

      --  If the match expression is null, we're only looking for the
      --  presence of a node, not its form. The result is always true.
      if Match_Expression = null then
         --  In the case of
         --     pick child().all(),
         --  child needs to be evaluated against the outer expression to
         --  be captured by the possible wrap or weave command.

         Push_Frame_Context;
         Top_Frame.Top_Context.Visit_Decision := Visit_Decision'Unchecked_Access;

         Push_Implicit_It (Browsed);

         if Top_Frame.Top_Context.Outer_Expr_Callback /= null then
            Top_Frame.Top_Context.Outer_Expr_Callback.all;
         end if;

         Pop_Object;
         Pop_Frame_Context;

         if Top_Frame.Top_Context.Yield_Callback /= null then
            Evaluate_Yield_Function;

            if Visit_Decision = Unknown then
               return Into;
            else
               return Visit_Decision;
            end if;
         else
            Result := new W_Reference_Type'
              (Value => W_Object (Browsed), others => <>);

            return Stop;
         end if;
      end if;

      --  There is a subtetly in the browsing functions. The It reference
      --  within these calls isn't the entity currently analyzed anymore but
      --  directly the entity that is being evaluated under these calls.
      --  However, we cannot create a sub frame as whatever we match needs
      --  to find its way to the command frame (otherwise any extracted
      --  group would be deleted upon frame popped).
      --  TODO: these specificities needs to be duly documented in the UG.
      Push_Implicit_It (Browsed);

      --  If there's a name capture above this expression, its value needs
      --  to be available in the underlying match expression. We only capture
      --  the entity outside of folding context. When folding, the result of
      --  the folding expression will actually be what needs to be captured.

      if Top_Frame.Top_Context.Name_Captured /= ""
        and then Top_Frame.Top_Context.Yield_Callback = null
      then
         Include_Symbol
           (To_Text (Top_Frame.Top_Context.Name_Captured),
            new W_Reference_Type'
              (Value => W_Object (Browsed), others => <>));
      end if;

      --  Prior to evaluating the expression, we need to remove potential name
      --  capture, as it would override the one we are capturing in this browsing
      --  iteration.

      Push_Frame_Context_Parameter_With_Match (W_Object (Browsed));
      Top_Frame.Top_Context.Name_Captured := To_Unbounded_Text ("");
      Top_Frame.Top_Context.Visit_Decision := Visit_Decision'Unchecked_Access;
      Top_Frame.Top_Context.Yield_Callback := null;

      Evaluate_Expression (Match_Expression);

      Pop_Frame_Context;

      Expression_Result := Pop_Object;

      Pop_Object;

      if Expression_Result /= Match_False then
         if Expression_Result.all in W_Reference_Type'Class
           and then W_Reference (Expression_Result).Is_Allocated
         then
            --  If we have a result only in allocated mode, and if this result
            --  is a new entity, this means that this new entity is actually
            --  the result of the browse, not the one searched.

            Result := Expression_Result;

            --  Note that it is illegal to call a fold function with an
            --  allocator in the fold expression (we would never know when to
            --  stop allocating). This case is supposed to have being taken
            --  care of earlier but raise an error here just in case.

            if Top_Frame.Top_Context.Yield_Callback /= null then
               Error ("allocation in yield browsing functions is illegal");
            end if;

            return Stop;
         else
            if Top_Frame.Top_Context.Yield_Callback /= null then
               Evaluate_Yield_Function;

               --  The result of the expansion can be calls to wrap functions,
               --  and one of this wrap function may be a visit decision. If
               --  that's the case, take it into account and reset the flag.

               if Visit_Decision = Unknown then
                  return Into;
               else
                  return Visit_Decision;
               end if;
            else
               Result := W_Object (Browsed);

               return Stop;
            end if;
         end if;
      else
         return Into;
      end if;
   end Browse_Entity;

   procedure Handle_Regexpr
     (Root      : access W_Object_Type'Class;
      Generator : access procedure
        (Node : access W_Object_Type'Class; Expr : T_Expr);
      Expr      : T_Expr;
      Outer_Process_Right : access procedure)
     with Post => Top_Frame.Data_Stack.Length
       = Top_Frame.Data_Stack.Length'Old + 1;

   procedure Evaluate_Generator_Regexp
     (Root      : access W_Object_Type'Class;
      Generator : access procedure
        (Node : access W_Object_Type'Class; Expr : T_Expr);
      Expr      : T_Expr)
   is
      Result_Variable : W_Regexpr_Result;

      procedure Capture_Callback (Mode : Capture_Mode) is
      begin
         if Mode = Capture then
            Result_Variable.Result.A_Vector.Append (Top_Object);
         else
            Result_Variable.Result.A_Vector.Delete_Last;
         end if;
      end Capture_Callback;

   begin
      --  There is no regexpr, just one expression. Compute it and return.

      if Expr = null
        or else Expr.Kind not in Template_Reg_Expr_Anchor | Template_Reg_Expr
      then
         Generator (Root, Expr);

         return;
      end if;

      --  We are on an actual regexpr. Install the result capture handler, and
      --  then process the expression

      Push_Frame_Context;

      Result_Variable := new W_Regexpr_Result_Type'(Result => new W_Vector_Type);

      if not Expr.Node.As_Reg_Expr.F_Captured.Is_Null then
         Top_Frame.Symbols.Insert (Expr.Node.As_Reg_Expr.F_Captured.Text, W_Object (Result_Variable));
      end if;

      Top_Frame.Top_Context.Capture_Callback := Capture_Callback'Unrestricted_Access;

      Handle_Regexpr (Root, Generator, Expr, null);

      Pop_Frame_Context;

      if Top_Object /= Match_False then
         Pop_Object;
         Push_Object (Result_Variable);
      end if;
   end Evaluate_Generator_Regexp;

   procedure Handle_Regexpr
     (Root      : access W_Object_Type'Class;
      Generator : access procedure
        (Node : access W_Object_Type'Class; Expr : T_Expr);
      Expr      : T_Expr;
      Outer_Process_Right : access procedure)
   is
      Quantifiers_Hit : Integer := 0;

      Original_Yield_Callback : Yield_Callback_Type := Top_Frame.Top_Context.Yield_Callback;

      Initial_Capture_Callback : Capture_Callback_Type;
      Captured_Variable : W_Regexpr_Result;

      procedure Capture_Callback (Mode : Capture_Mode) is
      begin
         Initial_Capture_Callback (Mode);

         if Mode = Capture then
            Captured_Variable.Result.A_Vector.Append (Top_Object);
         else
            Captured_Variable.Result.A_Vector.Delete_Last;
         end if;
      end Capture_Callback;

      procedure Yield_Action
        with Post => Top_Frame.Data_Stack.Length
          = Top_Frame.Data_Stack.Length'Old + 1;

      procedure Process_Right_Action
        with Post => Top_Frame.Data_Stack.Length
          = Top_Frame.Data_Stack.Length'Old + 1;

      procedure Install_Yield_Callback is
      begin
         Top_Frame.Top_Context.Yield_Callback := Yield_Action'Unrestricted_Access;
      end Install_Yield_Callback;

      procedure Restore_Yield_Callback is
      begin
         Top_Frame.Top_Context.Yield_Callback := Original_Yield_Callback;
      end Restore_Yield_Callback;

      procedure Process_Left_Expression (Object : access W_Object_Type'Class; Left_Expr : T_Expr) is
      begin
         --  This function is used to process the left side of a regular expression,
         --  which can be either a single expression, as in:
         --     A \ B (left is A)
         --  A subexpression as in:
         --     (A \ B) \ C (left is A \ B)
         --  Or a the expression of a quantifier
         --     many (A \ B) \ C (left is A \ B).
         --  It will call the generator on the left expression and trigger a
         --  yield of it result, calling then the yeild callback to process the
         --  right part.

         if Left_Expr.Kind = Template_Reg_Expr then
            --  We contain on a subexpression, for example in
            --     x: (A \ B) \ C
            --  We're on the regexpr that has "x: (A \ B)" on the left and "\ C"
            --  on the right. Call handle of the expression "x: (A \ B)" while
            --  passing a pointer to the "Process_Right_Action" call in order
            --  for the underlying expression analysis to call back into it
            --  at the end of its processing and carry on the analysis to \ C

            --  TODO: We're not using the value of Object. May not need it nor
            --  need the value of Root altogether.
            Handle_Regexpr (Top_Object, Generator, Left_Expr, Process_Right_Action'Access);
         else
            Install_Yield_Callback;
            Generator (Top_Object, Left_Expr);
            Restore_Yield_Callback;
         end if;
      end Process_Left_Expression;

      procedure Process_End_Of_Sub_Expression is
      begin
         --  This function has to be called when we reached the end of either
         --  an expression or a sub expression (which would be identified by the
         --  right prenthesis. If it's a sub-expression, it has an
         --  outer_process_right value, for example in
         --     x: (A \ B) \ C
         --  Reaching \ B) requires to call the outer right identified by \C
         --  Otherwise, we reached the end of the entire regular expression. If
         --  there is a function that has been set as a receiving values from
         --  a generator, e.g.:
         --     child (x: (A \ B) \ C).all()
         --  Then call that function (the original yield callback is set).
         --  Otherwise (the original yield callback is not set) stop the
         --  value generation by marking the visit decision to Stop.

         if Outer_Process_Right /= null then
            Outer_Process_Right.all;
         else
            Top_Frame.Top_Context.Capture_Callback (Capture);
            Push_Object (Top_Object);

            if Original_Yield_Callback /= null then
               Original_Yield_Callback.all;
               Delete_Object_At_Position (-2);
               Top_Frame.Top_Context.Visit_Decision.all := Into;
            else
               Top_Frame.Top_Context.Visit_Decision.all := Stop;
            end if;
         end if;
      end Process_End_Of_Sub_Expression;

      procedure Process_Right_Action is
         Result : W_Object;
      begin
         --  If we are not on the wrapper, Is_First_Matching_Wrapper is always true.
         --
         --  If we are on a wrapper, we only want to look at the next step
         --  in the iteration if we're on the first one that matches.
         --  Ignore the others.

         if Expr.Reg_Expr_Right /= null and then Top_Frame.Top_Context.Is_First_Matching_Wrapper then
            Top_Frame.Top_Context.Capture_Callback (Capture);
            Push_Frame_Context;

            --  If this expression is of the form "x (a \ b) \ c", when starting
            --  to analyze  c, the capturing function for x needs to be removed

            if Captured_Variable /= null then
               Top_Frame.Top_Context.Capture_Callback := Initial_Capture_Callback;
            end if;

            Restore_Yield_Callback;
            Handle_Regexpr (Top_Object, Generator, Expr.Reg_Expr_Right, Outer_Process_Right);
            Result := Top_Object.Dereference;

            Pop_Frame_Context;

            if Result = Match_False then
               Top_Frame.Top_Context.Capture_Callback (Rollback);
               Top_Frame.Top_Context.Visit_Decision.all := Into;
            else
               if Original_Yield_Callback /= null then
                  Top_Frame.Top_Context.Visit_Decision.all := Into;
               else
                  Top_Frame.Top_Context.Visit_Decision.all := Stop;
               end if;
            end if;
         elsif Expr.Reg_Expr_Right = null then
            Process_End_Of_Sub_Expression;
         else
            --  We're not at the end of the iteration but are not on the first
            --  mathing wrapper anymore. Don't continue the processing on this
            --  wrapper node, the next onces have already been processed.

            Push_Match_False;
         end if;
      end Process_Right_Action;

      procedure Yield_Action is
      begin
         Push_Frame_Context;

         if Expr.Kind = Template_Reg_Expr_Anchor then
            Top_Frame.Top_Context.Capture_Callback (Capture);
            Top_Frame.Top_Context.Yield_Callback := null;
            --  If we're evaluating an anchor at this stage, this is a right
            --  anchor. There should not be any more element available.

            Generator (Top_Object, null);

            if Pop_Object /= Match_False then
               Top_Frame.Top_Context.Capture_Callback (Rollback);
               Push_Match_False;
            else
               Push_Object (Root);-- TODO: Should probably not be root, but the
               --  result array instead
            end if;
         elsif Expr.Kind = Template_Reg_Expr then
            Top_Frame.Top_Context.Regexpr_Anchored := True;
            Install_Yield_Callback;

            if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier then
               Quantifiers_Hit := Quantifiers_Hit + 1;

               if Quantifiers_Hit < Expr.Reg_Expr_Left.Min then
                  --  First, no matter the quantifier, try to reach the minimum
                  --  value
                  Process_Left_Expression (Top_Object, Expr.Reg_Expr_Left.Quantifier_Expr);
               elsif Quantifiers_Hit = Expr.Reg_Expr_Left.Max then
                  -- Second, if we hit the max, move on to the right action
                  Process_Right_Action;
               else
                  --  Then if the quantifier is many, try to reach as many
                  --  as possible. If few, see if one is necessary.

                  case Expr.Reg_Expr_Left.Node.As_Reg_Expr_Quantifier.F_Quantifier.Kind is
                     when Template_Operator_Many =>
                        Process_Left_Expression (Top_Object, Expr.Reg_Expr_Left.Quantifier_Expr);

                        if Top_Object = Match_False then
                           --  If the result is false, we went one element too
                           --  far. Pop it and process the right action.

                           Pop_Object;
                           Process_Right_Action;
                        elsif Original_Yield_Callback /= null then
                           --  If we're doing an expansion, then we need to
                           --  consider all cases of potential children

                           Process_Right_Action;
                           Delete_Object_At_Position (-2);
                        end if;

                     when Template_Operator_Few =>
                        Process_Right_Action;

                        if Top_Object = Match_False then
                           --  If the result is false, the right action didn't
                           --  work, try to process another node under the
                           --  current condition

                           Pop_Object;
                           Process_Left_Expression (Top_Object, Expr.Reg_Expr_Left.Quantifier_Expr);
                        elsif Original_Yield_Callback /= null then
                           --  If we're doing an expansion, then we need to
                           --  consider all cases of potential children

                           Process_Left_Expression (Top_Object, Expr.Reg_Expr_Left.Quantifier_Expr);
                           Delete_Object_At_Position (-2);
                        end if;

                     when others =>
                        Error ("unexpected quantifier kind");
                  end case;
               end if;
            else
               Process_Right_Action;
            end if;
         else
            Process_End_Of_Sub_Expression;
         end if;

         Pop_Frame_Context;
      end Yield_Action;

      Result : W_Object;
   begin
      Push_Frame_Context;

      if Expr = null
        or else Expr.Kind not in Template_Reg_Expr_Anchor | Template_Reg_Expr
      then
         --  We are at the end of a subexpression, for example in
         --     x: (A \ B) \ C
         --  We reached either B or C. Generate the value for the expression to
         --  search for B in the first case, C in the second one. The yield
         --  callback will then drive either the end of the analysis or the
         --  processing of the rest of the expression.

         Install_Yield_Callback;
         Generator (Root, Expr);
         Restore_Yield_Callback;

         Pop_Frame_Context;

         return;
      end if;

      if Expr.Kind = Template_Reg_Expr then
         if not Expr.Node.As_Reg_Expr.F_Captured.Is_Null then
            Initial_Capture_Callback := Top_Frame.Top_Context.Capture_Callback;
            Captured_Variable := new W_Regexpr_Result_Type'(Result => new W_Vector_Type);
            Top_Frame.Symbols.Include (Expr.Node.As_Reg_Expr.F_Captured.Text, W_Object (Captured_Variable));
            Top_Frame.Top_Context.Capture_Callback := Capture_Callback'Unrestricted_Access;
         end if;
      end if;

      if Expr.Kind = Template_Reg_Expr_Anchor then
         Top_Frame.Top_Context.Yield_Callback := null;

         Generator (Root, null);

         Result := Pop_Object.Dereference;

         if Result = Match_False then
            Push_Object (Root);
         else
            Push_Match_False;
         end if;

         Top_Frame.Top_Context.Visit_Decision.all := Stop;
      else
         if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Anchor then
            Top_Frame.Top_Context.Regexpr_Anchored := True;
            Handle_Regexpr (Root, Generator, Expr.Reg_Expr_Right, Outer_Process_Right);
         elsif Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier then
            case Expr.Reg_Expr_Left.Node.As_Reg_Expr_Quantifier.F_Quantifier.Kind is
               when Template_Operator_Many =>
                  Process_Left_Expression (Root, Expr.Reg_Expr_Left.Quantifier_Expr);

                  if Top_Object = Match_False then
                     --  If we didn't find a match but the minimum requested is 0,
                     --  try to evaluate the result bypassing the quantifier.

                     if Expr.Reg_Expr_Left.Min = 0 then
                        Pop_Object;
                        Handle_Regexpr (Root, Generator, Expr.Reg_Expr_Right, Outer_Process_Right);
                     end if;
                  end if;

               when Template_Operator_Few =>
                  if Expr.Reg_Expr_Left.Min = 0 then
                     --  We are on a lazy quantifier and are accepting no matches.
                     --  First see if we can avoid the quantifier altogether

                     Handle_Regexpr (Root, Generator, Expr.Reg_Expr_Right, Outer_Process_Right);

                     if Top_Object = Match_False then
                        Pop_Object;

                        Process_Left_Expression (Root, Expr.Reg_Expr_Left.Quantifier_Expr);
                     end if;
                  else
                     Process_Left_Expression (Root, Expr.Reg_Expr_Left.Quantifier_Expr);
                  end if;

               when others =>
                  Error ("unexpected quantifier kind");
            end case;
         else
            Process_Left_Expression (Root, Expr.Reg_Expr_Left);
         end if;
      end if;

      Pop_Frame_Context;
   end Handle_Regexpr;

   procedure Push_Match_True (An_Entity : access W_Object_Type'Class) is
   begin
      Push_Object (An_Entity);
   end Push_Match_True;

   procedure Push_Match_False is
   begin
      Push_Object (Match_False);
   end Push_Match_False;

   procedure Include_Symbol (Name : Text_Type; Object : W_Object) is
   begin
      pragma Assert
        (if Object.all in W_Reference_Type'Class
         then W_Reference (Object).Value /= null);

      Top_Frame.Symbols.Include (Name, Object);
   end Include_Symbol;

   procedure Push_Call_Result
     (An_Entity : access W_Object_Type;
      Params    : T_Arg_Vectors.Vector) is
   begin
      Error
        ("non callable entity "
         & To_Wide_Wide_String
           (Ada.Tags.External_Tag
                (W_Object_Type'Class (An_Entity.all)'Tag)));
   end Push_Call_Result;

   procedure Generate_Values (Object : access W_Object_Type; Expr : T_Expr) is
   begin
      Push_Match_Result (W_Object (Object), Expr);

      if Top_Frame.Top_Context.Yield_Callback /= null then
         Top_Frame.Top_Context.Yield_Callback.all;
         Delete_Object_At_Position (-2);
      end if;
   end Generate_Values;

   function Match_With_Top_Object
     (An_Entity : access W_Object_Type) return Boolean
   is
      Other_Entity : W_Object := Top_Object.Dereference;
      Matched : Boolean;
   begin
      if Other_Entity = Match_False then
         return True;
      elsif Other_Entity.all in W_Regexp_Type'Class then
         Matched := Runtime.Analysis.Match
           (Other_Entity.To_String,
            W_Object_Type'Class (An_Entity.all).To_String);

         if not Matched then
            Pop_Object;
            Push_Match_False;
         end if;

         return True;
      elsif Other_Entity.all in W_Text_Expression_Type'Class then
         if Other_Entity.To_String /= W_Object_Type'Class (An_Entity.all).To_String then
            Pop_Object;
            Push_Match_False;
         end if;

         return True;
      elsif Other_Entity.all in W_Intrinsic_Function_Type'Class then
         --  Functions always match, their result is evaluated later.

         return True;
      end if;

      return False;
   end Match_With_Top_Object;

   function Lt
     (Left : access W_Object_Type; Right : access W_Object_Type'Class)
      return Boolean
   is
      Left_Tag : Tag := W_Object (Left).all'Tag;
      Right_Tag : Tag := Right.all'Tag;

      function To_Address is new Ada.Unchecked_Conversion (Tag, System.Address);
   begin
      if Left_Tag = Right_Tag then
         return Left.all'Address < Right.all'Address;
      else
         return To_Address (Left_Tag) < To_Address (Right_Tag);
      end if;
   end Lt;

   function Eq (Left : access W_Object_Type; Right : access W_Object_Type'Class) return Boolean is
   begin
      return Left = Right;
   end Eq;

   Object_For_Entity_Registry : W_Object_Maps.Map;

   function Get_Object_For_Entity
     (An_Entity : access T_Entity_Type'Class) return W_Object
   is
      Result : W_Template_Instance;
      Name : Text_Type := An_Entity.Full_Name;

      procedure Allocate_Variable (Var : T_Var) is
      begin
         case Var.Kind is
            when Map_Kind =>
               Result.Indexed_Variables.Insert
                 (Var.Name_Node.Text, new W_Reference_Type'
                    (Value => new W_Map_Type, others => <>));

            when Text_Kind =>
               --  Text is currently modelled as a reference to a text
               --  container.
               Result.Indexed_Variables.Insert
                 (Var.Name_Node.Text, new W_Reference_Type'
                    (Value => new W_Text_Vector_Type, others => <>));

            when others =>
               Error ("global variable type not yet supported");

         end case;
      end Allocate_Variable;

   begin
      if Object_For_Entity_Registry.Contains (Name) then
         return Object_For_Entity_Registry.Element (Name);
      else
         Result := new W_Template_Instance_Type;
         Result.Defining_Entity := T_Entity (An_Entity);

         if An_Entity.all in T_Module_Type'Class then
            for V of T_Module (An_Entity).Variables_Ordered loop
               Allocate_Variable (V);
            end loop;
         elsif An_Entity.all in T_Template_Type'Class then
            --  Templates are associated with a special vector of object, the
            --  registry, that contains all instances of the type.

            Result.Indexed_Variables.Insert
              ("_registry",
               new W_Reference_Type'(Value => new W_Vector_Type, others => <>));
         else
            Error ("static entity not associated with a node");
         end if;

         --  Static entities templates don't contain sections to be evaluated,
         --  and already have their symbols initialized.
         Result.Is_Evaluated := True;

         Object_For_Entity_Registry.Insert (Name, W_Object (Result));
         return W_Object (Result);
      end if;
   end Get_Object_For_Entity;

   function Make_Parameter
     (Name : Text_Type; Is_Optional : Boolean) return Parameter
   is
   begin
      return Parameter'
        (Name        => To_Unbounded_Text (Name),
         Is_Optional => Is_Optional);
   end Make_Parameter;

   function Process_Parameters
     (Profile : Parameter_Profile; Arg : T_Arg_Vectors.Vector) return Actuals_Type
   is
      Result : Actuals_Type (Profile'Range) := (others => null);

      Parameter_Index : Integer;
      In_Named_Section : Boolean := False;
      Formal : Parameter;
      Param : T_Arg;
   begin
      Parameter_Index := 1;

      for Actual_Index in 1 .. Arg.Length loop
         Param := Arg.Element (Integer (Actual_Index));

         if Param.Name /= "" then
            In_Named_Section := True;

            declare
               Name : Text_Type := To_Text (Param.Name);
               Found : Boolean := False;
            begin
               for I in Profile'Range loop
                  if Profile (I).Name = Name then
                     Parameter_Index := I;
                     Formal := Profile (Parameter_Index);
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Error ("parameter name '"  & Name & "' doesn't exit");
               end if;
            end;
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;

            Formal := Profile (Parameter_Index);
         end if;

         Result (Parameter_Index) := Param.Expr;

         Parameter_Index := Parameter_Index + 1;
      end loop;

      return Result;
   end Process_Parameters;

   procedure Push_Match_Result
     (Object              : W_Object;
      Matching_Expression : T_Expr)
   is
   begin
      if Matching_Expression = null then
         Push_Object (Object);
      else
         Push_Frame_Context_Parameter_With_Match (Object);

         Evaluate_Expression (Matching_Expression);

         if Pop_Object /= Match_False then
            Push_Object (Object);
         else
            Push_Match_False;
         end if;

         Pop_Frame_Context;
      end if;
   end Push_Match_Result;

   procedure Push_Match_It_Result
     (It                : W_Object;
      Matching_Expression : T_Expr) is
   begin
      if Matching_Expression = null then
         Push_Object (It);
      else
         Push_Frame_Context_Parameter_With_Match (It);

         Push_Implicit_It (It);
         Evaluate_Expression (Matching_Expression);

         if Pop_Object /= Match_False then
            Pop_Object; -- Pop It
            Push_Object (It);
         else
            Pop_Object; -- Pop It
            Push_Match_False;
         end if;

         Pop_Frame_Context;
      end if;
   end Push_Match_It_Result;

   procedure Handle_Call_Parameters
     (Args : T_Arg_Vectors.Vector;
      Evaluate_Parameter : access procedure
        (Name : Text_Type; Position : Integer; Value : T_Expr))
   is
      Parameter_Index : Integer;
      In_Named_Section : Boolean := False;
   begin
      Push_Frame_Context_Parameter;
      --  TODO: Do we really need expr callback here? Or maybe just reset to null?
      --  Which should probably always set to null when processing parameters...
      Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;

      Parameter_Index := 1;

      for Param of Args loop
         Push_Error_Location (Param.Node);

         if Param.Name /= "" then
            In_Named_Section := True;
            Evaluate_Parameter (Param.Name_Node.Text, Parameter_Index, Param.Expr);
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;

            Evaluate_Parameter ("", Parameter_Index, Param.Expr);
         end if;

         Pop_Error_Location;
         Parameter_Index := Parameter_Index + 1;
      end loop;

      Pop_Frame_Context;
   end Handle_Call_Parameters;


end Wrapping.Runtime.Structure;
