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
with System.Address_Image;

package body Wrapping.Runtime.Structure is

   Root_Language_Entities : W_Node_Maps.Map;

   ----------------
   -- Lt_Wrapper --
   ----------------

   function Lt_Wrapper (Left, Right : W_Object) return Boolean is
   begin
      return Left.Lt (Right);
   end Lt_Wrapper;

   ----------------
   -- Eq_Wrapper --
   ----------------

   function Eq_Wrapper (Left, Right : W_Object) return Boolean is
   begin
      return Left.Eq (Right);
   end Eq_Wrapper;

   ------------------------
   -- Get_Visible_Symbol --
   ------------------------

   function Get_Visible_Symbol (A_Frame: Data_Frame_Type; Name : Text_Type) return W_Object is
   begin
      if Top_Frame.Symbols.Contains (Name) then
         return Top_Frame.Symbols.Element (Name);
      end if;

      return null;
   end Get_Visible_Symbol;

   ----------------
   -- Get_Module --
   ----------------

   function Get_Module (A_Frame : Data_Frame_Type) return Semantic.Structure.T_Module is
      use Semantic.Structure;

      Scope : Semantic.Structure.T_Entity := A_Frame.Lexical_Scope;
   begin
      while Scope /= null and then Scope.all not in T_Module_Type'Class loop
         Scope := Scope.Parent;
      end loop;

      return Semantic.Structure.T_Module (Scope);
   end Get_Module;

   --------------
   -- Traverse --
   --------------

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

   -------------------
   -- Browse_Entity --
   -------------------

   function Browse_Entity
     (Browsed : access W_Object_Type'Class;
      Match_Expression : T_Expr;
      Result : out W_Object) return Visit_Action
   is
      Visit_Decision : aliased Visit_Action := Unknown;

      -----------------------------
      -- Evaluate_Yield_Function --
      -----------------------------

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
     with Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old + 1
     and Top_Frame.Top_Context = Top_Frame.Top_Context'Old;

   procedure Handle_Regexpr_Next_Value
     with Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old + 1;

   -------------------------------
   -- Evaluate_Generator_Regexp --
   -------------------------------

   procedure Evaluate_Generator_Regexp
     (Root      : access W_Object_Type'Class;
      Expr      : T_Expr;
      Generator : Generator_Type)
   is
      Result_Variable : W_Regexpr_Result;

      ----------------------
      -- Capture_Callback --
      ----------------------

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
         Push_Object (Root);
         Generator (Expr);
         Delete_Object_At_Position (-2);

         return;
      end if;

      --  We are on an actual regexpr. Install the result capture handler, and
      --  then process the expression
      declare
         Matcher : aliased Regexpr_Matcher_Type;
      begin
         Push_Frame_Context;

         Top_Frame.Top_Context.Regexpr := Matcher'Unchecked_Access;

         Result_Variable := new W_Regexpr_Result_Type'(Result => new W_Vector_Type);

         if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Anchor then
            Top_Frame.Top_Context.Regexpr_Anchored := True;
            Matcher.Current_Expr := Expr.Reg_Expr_Right;
         else
            Matcher.Current_Expr := Expr;
         end if;

         Matcher.Generator := Generator;
         Matcher.Overall_Yield_Callback := Top_Frame.Top_Context.Yield_Callback;
         Matcher.Capture_Callback := Capture_Callback'Unrestricted_Access;
         Matcher.Initial_Capture_Callback := Capture_Callback'Unrestricted_Access;

         Top_Frame.Top_Context.Yield_Callback := Handle_Regexpr'Access;

         Push_Object (Root);
         Handle_Regexpr_Next_Value;
         Delete_Object_At_Position (-2);

         Pop_Frame_Context;

         if Top_Object /= Match_False then
            Pop_Object;
            Push_Object (Result_Variable);
         end if;
      end;
   end Evaluate_Generator_Regexp;

   --------------------
   -- Handle_Regexpr --
   --------------------

   Depth : Integer := 0;

   function P return Wide_Wide_String is (Wide_Wide_String'(1 .. Depth => ' '));

   function Get_Right_Expression_Matcher (Allocated_Next_Matcher : Regexpr_Matcher) return Regexpr_Matcher is
      Matcher : Regexpr_Matcher := Top_Frame.Top_Context.Regexpr;
      Expr : T_Expr;
   begin
      if Matcher = null then
         return null;
      end if;

      Expr := Matcher.Current_Expr;

      if Expr.Kind not in Template_Reg_Expr or else Expr.Reg_Expr_Right = null then
         return Matcher.Outer_Next_Expr;
      else
         Allocated_Next_Matcher.all := Matcher.all;
         Allocated_Next_Matcher.Current_Expr := Expr.Reg_Expr_Right;
         return Allocated_Next_Matcher;
      end if;
   end Get_Right_Expression_Matcher;

   procedure Handle_Regexpr_Next_Value is
      Expr : T_Expr;
      Matcher : Regexpr_Matcher := Top_Frame.Top_Context.Regexpr;
      Next_Matcher : aliased Regexpr_Matcher_Type;
   begin
      if Top_Frame.Top_Context.Regexpr = null then
         --  We hit the end of the analysis.

         Push_Match_True (Top_Object);

         return;
      elsif not Top_Frame.Top_Context.Is_First_Matching_Wrapper then
         --  If we are not on the wrapper, Is_First_Matching_Wrapper is always true.
         --  If we are on a wrapper, we only want to look at the next step
         --  in the iteration if we're on the first one that matches.
         --  Ignore the others.
         --  We do however consider all leaves values which is the reason
         --  why this condition is after checking for the end of the iteration

         Push_Match_False;

         return;
      end if;

      Expr := Top_Frame.Top_Context.Regexpr.Current_Expr;

      if Expr.Kind = Template_Reg_Expr
        and then Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier
        and then Expr.Reg_Expr_Left.Node.As_Reg_Expr_Quantifier.F_Quantifier.Kind = Template_Operator_Few
        and then Top_Frame.Top_Context.Regexpr.Quantifiers_Hit >= Expr.Reg_Expr_Left.Min
      then
         --  We are on a 'few' quantifier and we hit the minimum required
         --  matches. Try to skip it and only execute the right edge.

         Push_Frame_Context;
         Top_Frame.Top_Context.Regexpr := Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);
         Handle_Regexpr_Next_Value;
         Pop_Frame_Context;

         if Top_Object /= Match_False then
            return;
         else
            Pop_Object;
         end if;
      end if;

      Matcher.Generator (null);

      if Expr.Kind = Template_Reg_Expr_Anchor then
         --  We are analyzing a right anchor. No element is expected to
         --  match from there. Invert the result, false if anything matched,
         --  true otherwise.

         if Pop_Object = Match_False then
            Push_Match_True (Top_Object);
         else
            Push_Match_False;
         end if;
      elsif (Top_Object = Match_False or else Matcher.Overall_Yield_Callback /= null)
        and then Expr.Kind = Template_Reg_Expr
        and then Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier
        and then Expr.Reg_Expr_Left.Node.As_Reg_Expr_Quantifier.F_Quantifier.Kind = Template_Operator_Many
        and then Top_Frame.Top_Context.Regexpr.Quantifiers_Hit >= Expr.Reg_Expr_Left.Min
      then
         Push_Frame_Context;

         Pop_Object;
         Top_Frame.Top_Context.Regexpr := Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);

         if Top_Frame.Top_Context.Regexpr /= null then
            Top_Frame.Top_Context.Regexpr.Capture_Callback := Top_Frame.Top_Context.Regexpr.Initial_Capture_Callback;
         end if;

         Handle_Regexpr_Next_Value;
         Pop_Frame_Context;
      end if;
   end Handle_Regexpr_Next_Value;

   procedure Handle_Regexpr is

      --  ------------------------------
      --  -- Process_Right_Expression --
      --  ------------------------------
      --
      --  procedure Process_Right_Expression (Generator_Decision : Visit_Action_Ptr) is
      --     Result : W_Object;
      --  begin
      --     --  If we are not on the wrapper, Is_First_Matching_Wrapper is always true.
      --     --
      --     --  If we are on a wrapper, we only want to look at the next step
      --     --  in the iteration if we're on the first one that matches.
      --     --  Ignore the others.
      --
      --     if Expr.Reg_Expr_Right /= null and then Top_Frame.Top_Context.Is_First_Matching_Wrapper then
      --        Push_Frame_Context;
      --
      --        --  If this expression is of the form "x (a \ b) \ c", when starting
      --        --  to analyze  c, the capturing function for x needs to be removed
      --
      --        if Captured_Variable /= null then
      --           Top_Frame.Top_Context.Capture_Callback := Initial_Capture_Callback;
      --        end if;
      --
      --        Restore_Overall_Yield_Callback;
      --        Handle_Regexpr (Expr.Reg_Expr_Right, Generator, Outer_Right_Expression, Generator_Decision);
      --        Result := Top_Object;
      --
      --        Pop_Frame_Context;
      --
      --        if Result = Match_False then
      --           Top_Frame.Top_Context.Capture_Callback (Rollback);
      --           Generator_Decision.all := Into;
      --        else
      --           if Overall_Yield_Callback /= null then
      --              Generator_Decision.all := Into;
      --           else
      --              Generator_Decision.all := Stop;
      --           end if;
      --        end if;
      --     elsif Expr.Reg_Expr_Right = null then
      --        Process_End_Of_Sub_Expression (Generator_Decision);
      --     else
      --        --  We're not at the end of the iteration but are not on the first
      --        --  mathing wrapper anymore. Don't continue the processing on this
      --        --  wrapper node, the next onces have already been processed.
      --
      --        Push_Match_False;
      --     end if;
      --  end Process_Right_Expression;

      Captured_Variable : W_Regexpr_Result;
      Sub_Matcher : aliased Regexpr_Matcher_Type;
      Next_Matcher : aliased Regexpr_Matcher_Type;
      Root_Capture_Callback : Boolean := False;

      ----------------------
      -- Capture_Callback --
      ----------------------

      procedure Capture_Callback (Mode : Capture_Mode) is
         Matcher : Regexpr_Matcher := Top_Frame.Top_Context.Regexpr;
      begin
         Matcher.Initial_Capture_Callback (Mode);

         if Mode = Capture then
            Captured_Variable.Result.A_Vector.Append (Top_Object);
         else
            Captured_Variable.Result.A_Vector.Delete_Last;
         end if;
      end Capture_Callback;

      procedure Control_Iteration;
      --  This procedure is to be called after the analysis of the current
      --  expression (left and right part), so when there's no more expression
      --  to analyze. It will control the overall iteration, stop it upon
      --  having found a result unless we're in a generator mode.

      procedure Control_Iteration is
         Matcher : Regexpr_Matcher := Top_Frame.Top_Context.Regexpr;
      begin
         if Top_Object /= Match_False then
            if Matcher.Overall_Yield_Callback /= null then
               Top_Frame.Top_Context.Visit_Decision.all := Into;
            else
               Top_Frame.Top_Context.Visit_Decision.all := Stop;
            end if;
         end if;
      end Control_Iteration;

      Matcher : Regexpr_Matcher := Top_Frame.Top_Context.Regexpr;
      Expr : T_Expr := Matcher.Current_Expr;
      Object : W_Object := Top_Object;

   begin
      if Top_Frame.Top_Context.Regexpr = null then
         Push_Match_True (Top_Object);

         if Matcher.Overall_Yield_Callback /= null then
            Matcher.Overall_Yield_Callback.all;
         end if;

         return;
      end if;

      Depth := Depth + 2;

      Push_Frame_Context;
      Top_Frame.Top_Context.Regexpr_Anchored := True;

      if Expr.Kind = Template_Reg_Expr_Anchor then
         --  We reached a right anchor. We should not have reached this stage.
         --  Push true inconditially. The outer call will then convert this
         --  into false. Note that we can't just push false here, otherwise
         --  the outer call could not be able to differenciate cases where
         --  false is received because an element has been found and cases where
         --  false is received because no element has been extracted (which
         --  would be correct).

         Push_Match_True (Top_Object);
      elsif Expr.Kind = Template_Reg_Expr then
         if not Matcher.Capture_Callback_Installed
           and then not Expr.Node.As_Reg_Expr.F_Captured.Is_Null
         then
            --  Capture callbacks are only available for expression with
            --  subexpressions. In the case of quantifier, we need to track
            --  which call actually installed the capture callabck to be
            --  able to remove it once all the quantification is done.

            Root_Capture_Callback := True;
            Matcher.Capture_Callback_Installed := True;
            Matcher.Initial_Capture_Callback := Matcher.Capture_Callback;
            Captured_Variable := new W_Regexpr_Result_Type'(Result => new W_Vector_Type);
            Top_Frame.Symbols.Include (Expr.Node.As_Reg_Expr.F_Captured.Text, W_Object (Captured_Variable));
            Matcher.Capture_Callback := Capture_Callback'Unrestricted_Access;
         end if;

         Sub_Matcher := Matcher.all;
         Sub_Matcher.Capture_Callback_Installed := False;

         if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier
           and then Expr.Reg_Expr_Left.Max > 0
           and then Matcher.Quantifiers_Hit >= Expr.Reg_Expr_Left.Max
         then
            -- If we hit the max, executing the quantifier is always false.
            -- Stack false and leave the caller to call right expression.

            Push_Match_False;
         else
            Push_Frame_Context;

            if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier then
               --  In the case of a quantifier, the outer expression to try
               --  is the qualifier itself, the left one is the Quantifer_Expr
               --  value.

               Sub_Matcher.Current_Expr := Expr.Reg_Expr_Left.Quantifier_Expr;
               Sub_Matcher.Outer_Next_Expr := Matcher;
            else
               --  In the case of a regular sub expression, the outer expression
               --  to try is the right expression of the current expression,
               --  the left one is in the field Reg_Expr_Left.

               Sub_Matcher.Current_Expr := Expr.Reg_Expr_Left;
               Sub_Matcher.Outer_Next_Expr := Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);
            end if;

            Top_Frame.Top_Context.Regexpr := Sub_Matcher'Unchecked_Access;
            Matcher.Quantifiers_Hit := Matcher.Quantifiers_Hit + 1;
            Handle_Regexpr;
            Matcher.Quantifiers_Hit := Matcher.Quantifiers_Hit - 1;
            Control_Iteration;

            Pop_Frame_Context;
         end if;

         if Root_Capture_Callback then
            Matcher.Capture_Callback := Matcher.Initial_Capture_Callback;
            Matcher.Capture_Callback_Installed := False;
            Matcher.Initial_Capture_Callback := null;
         end if;
      else
         if not Evaluate_Match_Result (Object, Expr) then
            Push_Match_False;
         else
            Matcher.Capture_Callback (Capture);

            Push_Frame_Context;
            Top_Frame.Top_Context.Regexpr := Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);

            if Top_Frame.Top_Context.Regexpr /= null then
               Top_Frame.Top_Context.Yield_Callback := Handle_Regexpr'Access;
               Handle_Regexpr_Next_Value;
               Control_Iteration;

               --  TODO: This may not be enough in the context of an all ()
               --  generation as we'll also want to rollback when exploring
               --  other
               if Top_Object = Match_False then
                  Matcher.Capture_Callback (Rollback);
               end if;

               Pop_Frame_Context;
            else
               Pop_Frame_Context;
               Push_Match_True (Top_Object);
               Control_Iteration;

               if Matcher.Overall_Yield_Callback /= null then
                  Matcher.Overall_Yield_Callback.all;
                  Delete_Object_At_Position (-2);
               end if;
            end if;
         end if;
      end if;

      Depth := Depth - 2;
      Pop_Frame_Context;
   end Handle_Regexpr;

   ---------------------
   -- Push_Match_True --
   ---------------------

   procedure Push_Match_True (An_Entity : access W_Object_Type'Class) is
   begin
      Push_Object (An_Entity);
   end Push_Match_True;

   ----------------------
   -- Push_Match_False --
   ----------------------

   procedure Push_Match_False is
   begin
      Push_Object (Match_False);
   end Push_Match_False;

   --------------------
   -- Include_Symbol --
   --------------------

   procedure Include_Symbol (Name : Text_Type; Object : not null W_Object) is
   begin
      pragma Assert
        (if Object.all in W_Reference_Type'Class
         then W_Reference (Object).Value /= null);

      Top_Frame.Symbols.Include (Name, Object);
   end Include_Symbol;

   ----------------------
   -- Push_Call_Result --
   ----------------------

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

   ---------------------
   -- Generate_Values --
   ---------------------

   procedure Generate_Values (Object : access W_Object_Type; Expr : T_Expr) is
   begin
      Push_Match_Result (W_Object (Object), Expr);

      if Top_Frame.Top_Context.Yield_Callback /= null then
         Top_Frame.Top_Context.Yield_Callback.all;
         Delete_Object_At_Position (-2);
      end if;
   end Generate_Values;

   ---------------------------
   -- Match_With_Top_Object --
   ---------------------------

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

   --------
   -- Lt --
   --------

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

   --------
   -- Eq --
   --------

   function Eq (Left : access W_Object_Type; Right : access W_Object_Type'Class) return Boolean is
   begin
      return Left = Right;
   end Eq;

   Object_For_Entity_Registry : W_Object_Maps.Map;

   ---------------------------
   -- Get_Object_For_Entity --
   ---------------------------

   function Get_Object_For_Entity
     (An_Entity : access T_Entity_Type'Class) return W_Object
   is
      Result : W_Template_Instance;
      Name : Text_Type := An_Entity.Full_Name;

      -----------------------
      -- Allocate_Variable --
      -----------------------

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

   --------------------
   -- Make_Parameter --
   --------------------

   function Make_Parameter
     (Name : Text_Type; Is_Optional : Boolean) return Parameter
   is
   begin
      return Parameter'
        (Name        => To_Unbounded_Text (Name),
         Is_Optional => Is_Optional);
   end Make_Parameter;

   ------------------------
   -- Process_Parameters --
   ------------------------

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

   ---------------------------
   -- Evaluate_Match_Result --
   ---------------------------

   function Evaluate_Match_Result
     (Object : W_Object;
      Matching_Expression : T_Expr) return Boolean
   is
   begin
      Push_Match_Result (Object, Matching_Expression);

      return Pop_Object /= Match_False;
   end Evaluate_Match_Result;

   -----------------------
   -- Push_Match_Result --
   -----------------------

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

   --------------------------
   -- Push_Match_It_Result --
   --------------------------

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

   ----------------------------
   -- Handle_Call_Parameters --
   ----------------------------

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
