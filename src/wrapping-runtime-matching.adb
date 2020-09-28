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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with Libtemplatelang.Common; use Libtemplatelang.Common;

with Wrapping.Regex;               use Wrapping.Regex;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;
with Wrapping.Runtime.Objects;     use Wrapping.Runtime.Objects;

package body Wrapping.Runtime.Matching is

   procedure Handle_Regexpr_Value with
     Pre  => Top_Context.Regexpr /= null,
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Analyze a value that has been generated through iteration agains the
   --  current element of the regular expression.

   procedure Handle_Regexpr_Generate_Next_Value with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Calls the current regular expression generator to generate the next
   --  value to analyze.

   procedure Capture_Value (Capturing : Capture_Result; Mode : Capture_Mode);
   --  When mode is Capture, recursively adds Top_Object to all the capturing
   --  object. When it's Rollback, recursively removes the last object added
   --  to all capturing objects.

   function Get_Right_Expression_Matcher
     (Allocated_Next_Matcher : Regexpr_Matcher) return Regexpr_Matcher;
   --  Return the right expression matcher. If we are before the end of a
   --  subexpression, e.g. in:
   --     (a \ b) \ c
   --  we're on a, will return Allocated_Next_Matcher set to b. If we are at
   --  the end of the subexpression, b on the previous example, then will
   --  return the outer next matched which is supposed to have already been
   --  set

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

   -----------------------
   -- Push_Match_Result --
   -----------------------

   procedure Push_Match_Result
     (Matching_Expression : T_Expr;
      Object : W_Object := Top_Object)
   is
   begin
      if Matching_Expression = null then
         --  If there's no matching expression, the result is always true.

         Push_Object (Object);
      else
         --  Create a context that will match against the object in parameter

         Push_Frame_Context_Parameter_With_Match (Object);

         --  Push the object in parameter if the result of the matching
         --  expression is true, false otherwise.

         if Evaluate_Expression (Matching_Expression) /= Match_False then
            Push_Object (Object);
         else
            Push_Match_False;
         end if;

         Pop_Frame_Context;
      end if;
   end Push_Match_Result;

   ---------------------------
   -- Evaluate_Match_Result --
   ---------------------------

   function Evaluate_Match
     (Matching_Expression : T_Expr;
      Object : W_Object := Top_Object) return Boolean
   is
   begin
      Push_Match_Result (Matching_Expression, Object);

      return Pop_Object /= Match_False;
   end Evaluate_Match;

   -----------
   -- Match --
   -----------

   function Match (Pattern, Text : Text_Type) return Boolean is
      Text_Str : constant String    := To_String (Text);
      Matches  : constant Match_Obj :=
        Match (Compile (To_String (Pattern)), Text_Str);
   begin
      --  If the pattern didn't match the text, return false.

      if No_Match (Matches) then
         return False;
      end if;

      --  Otherwise, go through all the group captured, add them to the list
      --  of captured group of the current frame. If they have a name, also
      --  add them to the list of symbols.

      for I in 1 .. Matches.Matches.Element'Last loop
         declare
            Matched_Text : constant Text_Type :=
              To_Text
                (Text_Str
                   (Matches.Matches.Element (I).First ..
                        Matches.Matches.Element (I).Last));
            Name : constant Text_Type :=
              To_Text (Get_Capture_Name (Matches, I));
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

   -------------------
   -- Capture_Value --
   -------------------

   procedure Capture_Value (Capturing : Capture_Result; Mode : Capture_Mode) is
   begin
      if Capturing.Parent /= null then
         --  Captured value may be enclosing one another, e.g.:
         --     x: (a \ (y: b \ c))
         --  In the above example, when capturing b and c, both x and y should
         --  get the value or rollback.

         Capture_Value (Capturing.Parent, Mode);
      end if;

      if Mode = Capture then
         --  If we're capturing, add the value to the resulting captured
         --  object.

         W_Regexpr_Result (Capturing.Object).Result.A_Vector.Append
           (Top_Object);
      else
         --  Othwerwise, remove the last pushed value

         W_Regexpr_Result (Capturing.Object).Result.A_Vector.Delete_Last;
      end if;
   end Capture_Value;

   -------------------------------
   -- Evaluate_Generator_Regexp --
   -------------------------------

   procedure Evaluate_Generator_Regexp
     (Root      : access W_Object_Type'Class; Expr : T_Expr;
      Generator : Generator_Callback_Type)
   is
      Result_Variable : aliased Capture_Result_Type;
   begin
      Push_Frame_Context;
      Push_Implicit_It (Root);

      --  There is no regexpr, e.g. we're in a state like:
      --     child (x)
      --  which is a simple expression, as opposed to
      --     child (x \ y)
      --  which would have at least two elements to chech
      --  In this case, just call the generator checking for that one
      --  expression.

      if Expr = null
        or else Expr.Kind not in Template_Reg_Expr_Anchor | Template_Reg_Expr
      then
         Generator (Expr);
         Pop_Underneath_Top;
         Pop_Frame_Context;

         return;
      end if;

      --  We are on a complex regexpr. Install the result capture handler, and
      --  then process the expression

      declare
         Matcher : aliased Regexpr_Matcher_Type;
      begin
         Push_Frame_Context;

         --  Stacks the matcher for the first element of that expression

         Top_Context.Regexpr := Matcher'Unchecked_Access;

         --  Creates an object to capture the result of the overall expression.

         Result_Variable.Object :=
           new W_Regexpr_Result_Type'(Result => new W_Vector_Type);

         if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Anchor then
            --  If we have a left anchor, that is we are in a case that
            --  looks like:
            --     \ a \ b
            --  as opposed to
            --     a \ b
            --  In that case, mark the regexpr as being anchored (the
            --  first level of generator should match, e.g. if it's a child,
            --  the first level of child should match). Move the current expr
            --  of the matcher to the next part of expression, a \ b in the
            --  above case.

            Top_Context.Regexpr_Anchored := True;
            Matcher.Current_Expr := Expr.Reg_Expr_Right;
         else
            --  We're not anchored yet, the current expression of this matcher
            --  is the expression itself.

            Matcher.Current_Expr := Expr;
         end if;

         --  Sets other aspects of this matcher

         Matcher.Generator := Generator;
         Matcher.Capturing := Result_Variable'Unchecked_Access;

         --  When entering a regular expression, the generator will yield
         --  values that will be captured and analyzed by the regexpr algorithm
         --  in Handle_Regexpr_Value. The yeild callback that was present when
         --  entering the analysis, if any, needs to be called at the very end.
         --  Store the original callback.

         Matcher.Overall_Yield_Callback := Top_Context.Yield_Callback;

         --  Upon processing, the regular expression engine modifies the
         --  visit decision outcome as to stop when it found a value. However
         --  in some cases, we don't even stack a visit decision prior to
         --  stoping the analysis. In this case, there's no decision to take,
         --  push null at the top. Failing to do so will let such decision
         --  stop potential iterations outside of the regular expression.

         Top_Context.Visit_Decision := null;

         --  Launch the regular expression analysis by generating the next
         --  value.

         Handle_Regexpr_Generate_Next_Value;
         Pop_Underneath_Top;

         Pop_Frame_Context;

         if Top_Object /= Match_False then
            --  If we match, we want to return the full captured object, not
            --  just the value at the top of the stack.

            Pop_Object;
            Push_Object (Result_Variable.Object);
         end if;
      end;

      Pop_Frame_Context;
   end Evaluate_Generator_Regexp;

   ----------------------------------
   -- Get_Right_Expression_Matcher --
   ----------------------------------

   function Get_Right_Expression_Matcher
     (Allocated_Next_Matcher : Regexpr_Matcher) return Regexpr_Matcher
   is
      Matcher : constant Regexpr_Matcher := Top_Context.Regexpr;
      Expr    : T_Expr;
   begin
      if Matcher = null then
         return null;
      end if;

      Expr := Matcher.Current_Expr;

      if Expr.Kind in Template_Reg_Expr
        and then Expr.Reg_Expr_Right /= null
      then
         --  There are still expressions to analyze on this subexpression, e.g.
         --  on:
         --     (a \ b) \ c
         --  we're on a. Get b.

         Allocated_Next_Matcher.all          := Matcher.all;
         Allocated_Next_Matcher.Current_Expr := Expr.Reg_Expr_Right;

         if Matcher.Capture_Installed then
            Allocated_Next_Matcher.Capturing := Matcher.Capturing.Parent;
         end if;

         return Allocated_Next_Matcher;
      else
         --  There are no more expressions to analyze on this subexpression,
         --  e.g.  on:
         --     (a \ b) \ c
         --  we're on ab. Get c.

         return Matcher.Outer_Next_Expr;
      end if;
   end Get_Right_Expression_Matcher;

   -------------------------------
   -- Handle_Regexpr_Next_Value --
   -------------------------------

   procedure Handle_Regexpr_Generate_Next_Value is
      Expr         : T_Expr;
      Matcher      : constant Regexpr_Matcher := Top_Context.Regexpr;
      Next_Matcher : aliased Regexpr_Matcher_Type;
   begin
      if Top_Context.Regexpr = null then
         --  We hit the end of the analysis without generating any value. End
         --  the analysis with success. This may happen for example on:
         --     many (x)
         --  where 0 matches is an acceptable outcome.

         Push_Match_True (Top_Object);

         return;
      elsif not Top_Context.Is_First_Matching_Wrapper then
         --  Regular expressions may be performed on trees of wrapper.
         --  If we are not on the wrapper, Is_First_Matching_Wrapper is always
         --  true. If we are on a wrapper, we only want to look at the next
         --  step in the iteration if we're on the first one that matches.
         --  Ignore the others. We do however consider all leaves values which
         --  is the reason why this condition is after checking for the end of
         --  the iteration

         Push_Match_False;

         return;
      end if;

      Expr := Top_Context.Regexpr.Current_Expr;

      if Expr.Kind = Template_Reg_Expr
        and then Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier
        and then
          Expr.Reg_Expr_Left.Node.As_Reg_Expr_Quantifier.F_Quantifier.Kind =
          Template_Operator_Few
        and then Top_Context.Regexpr.Quantifiers_Hit >=
          Expr.Reg_Expr_Left.Quantifier_Min
      then
         --  The current expression is a "few" quantifier that hit
         --  the minimum expected matches, then we should first try to run
         --  the left expression (this is a lazy quantifier, should try to not
         --  take it). If it matches, then return, otherwise dimiss the result
         --  and try to process the quantifier.

         Push_Frame_Context;
         Top_Context.Regexpr :=
           Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);
         Handle_Regexpr_Generate_Next_Value;
         Pop_Frame_Context;

         if Top_Object /= Match_False then
            return;
         else
            Pop_Object;
         end if;
      end if;

      --  Sets the Yield callback to Handle_Regexpr_Value which will analyze
      --  the generated value according to the current matcher.
      Top_Context.Yield_Callback := Handle_Regexpr_Value'Access;

      --  Generate a new value, without any matching expression. The matching
      --  expression will be retreived and analyzed within
      --  Handle_Regexpr_Value.
      Matcher.Generator (null);

      if Expr.Kind = Template_Reg_Expr_Anchor then
         --  We are analyzing a right anchor. No element is expected to match
         --  from there. If we did get a match, push false, otherwise push
         --  true.

         if Pop_Object = Match_False then
            Push_Match_True (Top_Object);
         else
            Push_Match_False;
         end if;
      elsif
        (Top_Object = Match_False
         or else Matcher.Overall_Yield_Callback /= null)
        and then Expr.Kind = Template_Reg_Expr
        and then Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier
        and then
          Expr.Reg_Expr_Left.Node.As_Reg_Expr_Quantifier.F_Quantifier.Kind =
          Template_Operator_Many
        and then Top_Context.Regexpr.Quantifiers_Hit >=
          Expr.Reg_Expr_Left.Quantifier_Min
      then
         --  The current expression is a "many" quantifier that hit
         --  the minimum expected matches, and couldn't run its own
         --  sub-expression. It's time to move on to the right expression.

         Push_Frame_Context;

         Pop_Object;
         Top_Context.Regexpr :=
           Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);

         Handle_Regexpr_Generate_Next_Value;
         Pop_Frame_Context;
      end if;
   end Handle_Regexpr_Generate_Next_Value;

   --------------------
   -- Handle_Regexpr --
   --------------------

   procedure Handle_Regexpr_Value is

      procedure Control_Iteration;
      --  This procedure is to be called after the analysis of the current
      --  expression (left and right part), so when there's no more expression
      --  to analyze. It will control the overall iteration, stop it upon
      --  having found a result unless we're in a generator mode.

      Matcher : constant Regexpr_Matcher := Top_Context.Regexpr;

      -----------------------
      -- Control_Iteration --
      -----------------------

      procedure Control_Iteration is
         Matcher : constant Regexpr_Matcher := Top_Context.Regexpr;
      begin
         --  If we found a match and are not trying to generate all possible
         --  values, stop the iteration, otherwise carry on looking into.

         if Top_Object /= Match_False then
            if Top_Context.Visit_Decision /= null then
               if Matcher.Overall_Yield_Callback /= null then
                  Top_Context.Visit_Decision.all := Into;
               else
                  Top_Context.Visit_Decision.all := Stop;
               end if;
            end if;
         end if;
      end Control_Iteration;

      Captured_Variable     : aliased Capture_Result_Type;

      Sub_Matcher           : aliased Regexpr_Matcher_Type;
      --  We allocate matchers on the stack instead of the heap in order to
      --  have easy memory reclaim. This matcher is used for the subexpression.

      Next_Matcher          : aliased Regexpr_Matcher_Type;
      --  We allocate matchers on the stack instead of the heap in order to
      --  have easy memory reclaim. This matcher is used for the next
      --  expression.

      Root_Capture_Callback : Boolean         := False;
      Expr                  : constant T_Expr := Matcher.Current_Expr;

   begin
      Push_Frame_Context;

      --  Analyze the current value. Three cases:
      --     - the current expression is a right anchor, end the processing
      --     - the current expression is still a regular expression. It's too
      --       early to analyze the current node, go down in the regular
      --       expression tree.
      --     - the current expression is a simple expression, match the current
      --       value against it and move the analysis one step.

      --  At this stage, we generated at least one value, so in
      --     a \ b
      --  we're analyzing a and will then move to \ b. This will be anchored
      --  (e.g. b will need to match the value directly generated after the
      --  one matching a). So set the anchor to true.

      Top_Context.Regexpr_Anchored := True;

      if Expr.Kind = Template_Reg_Expr_Anchor then
         --  We reached a right anchor, which means that we expected the
         --  previous step to the the end of the analysis. We should not have
         --  been able to generate a value and reach this stage.
         --  Push true inconditially. The outer call will then convert this
         --  into false. Note that we can't just push false here, otherwise the
         --  outer call could not be able to differenciate cases where false is
         --  received because an element has been found and cases where false
         --  is received because no element has been extracted (which would be
         --  correct).

         Push_Match_True (Top_Object);
      elsif Expr.Kind = Template_Reg_Expr then
         --  The current expression is a regular expression. We can't analyze
         --  the current value yet. Instead, initialize the Sub_Matcher to
         --  the correct sub expression, and recursively call
         --  Handle_Regexpr_Value on that sub expression in order to do the
         --  proper matching.

         if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier
           and then Expr.Reg_Expr_Left.Quantifier_Max > 0
           and then Matcher.Quantifiers_Hit >=
             Expr.Reg_Expr_Left.Quantifier_Max
         then
            --  We are on a quantifier that has a maximum number of occurences.
            --  If we hit the max, executing the quantifier is always false.
            --  Stack false and leave the caller to switch to the next part
            --  of the expression if any.

            Push_Match_False;
         else
            if not Matcher.Capture_Installed
              and then not Expr.Node.As_Reg_Expr.F_Captured.Is_Null
            then
               --  We are on a subexpression that is capturing the result, e.g.
               --     x: (a \ b)
               --  or
               --     x: many (a)
               --  Create the capturing object.

               --  In the case of quantifier, as
               --  we're going to call Handle_Regexpr_Value with the same
               --  expression many times, we need to track if the
               --  capturing object was already installed as to be able not
               --  to create more than one and to remove it once all the
               --  quantification is done.

               Root_Capture_Callback     := True;
               Matcher.Capture_Installed := True;

               --  Create the capturing object, setting its parent to the
               --  previously installed capturing object (there's always one,
               --  either the overall capturing or another nested one).

               Captured_Variable.Object  :=
                 new W_Regexpr_Result_Type'(Result => new W_Vector_Type);
               Captured_Variable.Parent := Matcher.Capturing;
               Matcher.Capturing        := Captured_Variable'Unchecked_Access;

               --  Add this new object to the symbol in order to be able to
               --  refer to it within the subexpression. E.g. one can write:
               --     x: (a \ x.something)

               Top_Frame.Symbols.Include
                 (Expr.Node.As_Reg_Expr.F_Captured.Text,
                  Captured_Variable.Object);
            end if;

            Push_Frame_Context;

            --  Create the subexpression matcher as a copy of the current
            --  matcher (but allow this subexpression to install its own
            --  capture object).

            Sub_Matcher                   := Matcher.all;
            Sub_Matcher.Capture_Installed := False;

            if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier then
               --  In the case of a quantifier, the outer expression to try
               --  is the qualifier itself - in other words, in something like:
               --     many (a)
               --  we model the matcher as being:
               --     a [current expression] \ many (a) [next expression]
               --  We already have a matcher for the next expression, keeping
               --  the same one that was already installed and setup/
               Sub_Matcher.Current_Expr := Expr.Reg_Expr_Left.Quantifier_Expr;
               Sub_Matcher.Outer_Next_Expr := Matcher;
            else
               --  In the case of a regular sub expression, e.g.:
               --     a \ b
               --  the next expression to try is the right expression of the
               --  current expression, the left one is in the field
               --  Reg_Expr_Left, e.g.:
               --     a [cuurrent expression] \ b [next expression]
               --  We need a new matcher for the next expression (b), so using
               --  the Next_Matcher created on this function.

               Sub_Matcher.Current_Expr    := Expr.Reg_Expr_Left;
               Sub_Matcher.Outer_Next_Expr :=
                 Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);
               Sub_Matcher.Outer_Next_Expr.Capture_Installed := False;
            end if;

            --  Sets matcher for the subexpression to a new matcher created on
            --  this function.

            Top_Context.Regexpr     := Sub_Matcher'Unchecked_Access;

            --  In the case of quantifiers, we need to count how many times
            --  they've been hit as to detect if the minium criteria has being
            --  met, and to stop when we hit the max.

            Matcher.Quantifiers_Hit := Matcher.Quantifiers_Hit + 1;

            --  Recursively call Handle_Regexpr_Value on the subexpression that
            --  was set by Top_Context.Regexpr

            Handle_Regexpr_Value;

            --  Keeps tracks of how many quantifiers hits we've had on this
            --  specific call

            Matcher.Quantifiers_Hit := Matcher.Quantifiers_Hit - 1;

            --  Decides if we need to interrupt the iteration, that is if we
            --  found a result and we're not trying to generate all possible
            --  results.

            Control_Iteration;

            Pop_Frame_Context;

            --  If this frame was installing the capture callback, then remove
            --  it.
            --  TODO: We also need to restore the symbol here.

            if Root_Capture_Callback then
               Matcher.Capturing         := Matcher.Capturing.Parent;
               Matcher.Capture_Installed := False;
            end if;
         end if;
      else
         --  We are on a simple expression on which the object can be matched

         if not Evaluate_Match (Expr) then
            --  The top object didn't match that expression, push false

            Push_Match_False;
         else
            --  The top object matched.

            --  Capture the result

            Capture_Value (Matcher.Capturing, Capture);

            Push_Frame_Context;

            --  Retreives the matcher for the next expression to match

            Top_Context.Regexpr :=
              Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);

            if Top_Context.Regexpr /= null then
               --  Generates the next value

               Handle_Regexpr_Generate_Next_Value;

               --  Interrupt the iteration if needed

               Control_Iteration;

               --  If we didn't match, rollback the value that was captured
               --  earlier.

               --  TODO: This may not be enough in the context of an all ()
               --  generation as we'll also want to rollback when exploring
               --  other
               if Top_Object = Match_False then
                  Capture_Value (Matcher.Capturing, Rollback);
               end if;

               Pop_Frame_Context;
            else
               --  There's no more expression to analyze (ie
               --  Get_Right_Expression returned null). We sucessfully went
               --  through the  entire analysis. Push Match true, mark the
               --  end of the iteration if needed and call the original
               --  yield callback if there was a yeild when entering the root
               --  of the regular expression.

               Pop_Frame_Context;
               Push_Match_True (Top_Object);
               Control_Iteration;

               if Matcher.Overall_Yield_Callback /= null then
                  Matcher.Overall_Yield_Callback.all;
                  Pop_Underneath_Top;
               end if;
            end if;
         end if;
      end if;

      Pop_Frame_Context;
   end Handle_Regexpr_Value;

end Wrapping.Runtime.Matching;
