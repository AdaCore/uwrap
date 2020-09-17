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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers;             use Ada.Containers;

with Libtemplatelang.Common; use Libtemplatelang.Common;

with Wrapping.Regex;               use Wrapping.Regex;
with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;
with Wrapping.Runtime.Objects;     use Wrapping.Runtime.Objects;

package body Wrapping.Runtime.Matching is

   procedure Handle_Regexpr with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   procedure Handle_Regexpr_Next_Value with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;

   procedure Capture_Value (Capturing : Capture_Result; Mode : Capture_Mode);

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

   -----------------------
   -- Push_Match_Result --
   -----------------------

   procedure Push_Match_Result
     (Matching_Expression : T_Expr;
      Object : W_Object := Top_Object)
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

   -------------------
   -- Capture_Value --
   -------------------

   procedure Capture_Value (Capturing : Capture_Result; Mode : Capture_Mode) is
   begin
      if Capturing.Parent /= null then
         Capture_Value (Capturing.Parent, Mode);
      end if;

      if Mode = Capture then
         W_Regexpr_Result (Capturing.Object).Result.A_Vector.Append
           (Top_Object);
      else
         W_Regexpr_Result (Capturing.Object).Result.A_Vector.Delete_Last;
      end if;
   end Capture_Value;

   -------------------------------
   -- Evaluate_Generator_Regexp --
   -------------------------------

   procedure Evaluate_Generator_Regexp
     (Root      : access W_Object_Type'Class; Expr : T_Expr;
      Generator : Generator_Type)
   is
      Result_Variable : aliased Capture_Result_Type;
      Dummy_Generation_Control : aliased Visit_Action;
   begin
      Push_Implicit_It (Root);

      --  There is no regexpr, just one expression. Compute it and return.

      if Expr = null
        or else Expr.Kind not in Template_Reg_Expr_Anchor | Template_Reg_Expr
      then
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

         Top_Context.Regexpr := Matcher'Unchecked_Access;

         Result_Variable.Object :=
           new W_Regexpr_Result_Type'(Result => new W_Vector_Type);

         if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Anchor then
            Top_Context.Regexpr_Anchored := True;
            Matcher.Current_Expr                   := Expr.Reg_Expr_Right;
         else
            Matcher.Current_Expr := Expr;
         end if;

         Matcher.Generator              := Generator;
         Matcher.Overall_Yield_Callback := Top_Context.Yield_Callback;
         Matcher.Capturing := Result_Variable'Unchecked_Access;

         Top_Context.Yield_Callback := Handle_Regexpr'Access;

         --  Upon processing, the regular expression engine modifies the
         --  visit decision outcome. Make sure it doesn't modify it for the
         --  above iteration in case there's no generation.
         Top_Context.Visit_Decision :=
           Dummy_Generation_Control'Unchecked_Access;

         Handle_Regexpr_Next_Value;
         Delete_Object_At_Position (-2);

         Pop_Frame_Context;

         if Top_Object /= Match_False then
            Pop_Object;
            Push_Object (Result_Variable.Object);
         end if;
      end;
   end Evaluate_Generator_Regexp;

   -------------------------------
   -- Handle_Regexpr_Next_Value --
   -------------------------------

   function Get_Right_Expression_Matcher
     (Allocated_Next_Matcher : Regexpr_Matcher) return Regexpr_Matcher
   is
      Matcher : Regexpr_Matcher := Top_Context.Regexpr;
      Expr    : T_Expr;
   begin
      if Matcher = null then
         return null;
      end if;

      Expr := Matcher.Current_Expr;

      if Expr.Kind not in Template_Reg_Expr or else Expr.Reg_Expr_Right = null
      then
         return Matcher.Outer_Next_Expr;
      else
         Allocated_Next_Matcher.all          := Matcher.all;
         Allocated_Next_Matcher.Current_Expr := Expr.Reg_Expr_Right;

         if Matcher.Capture_Installed then
            Allocated_Next_Matcher.Capturing := Matcher.Capturing.Parent;
         end if;

         return Allocated_Next_Matcher;
      end if;
   end Get_Right_Expression_Matcher;

   procedure Handle_Regexpr_Next_Value is
      Expr         : T_Expr;
      Matcher      : Regexpr_Matcher := Top_Context.Regexpr;
      Next_Matcher : aliased Regexpr_Matcher_Type;
   begin
      if Top_Context.Regexpr = null then
         --  We hit the end of the analysis.

         Push_Match_True (Top_Object);

         return;
      elsif not Top_Context.Is_First_Matching_Wrapper then
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
          Expr.Reg_Expr_Left.Min
      then
         --  We are on a 'few' quantifier and we hit the minimum required
         --  matches. Try to skip it and only execute the right edge.

         Push_Frame_Context;
         Top_Context.Regexpr :=
           Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);
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
         --  We are analyzing a right anchor. No element is expected to match
         --  from there. Invert the result, false if anything matched, true
         --  otherwise.

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
        and then Top_Context.Regexpr.Quantifiers_Hit >= Expr.Reg_Expr_Left.Min
      then
         Push_Frame_Context;

         Pop_Object;
         Top_Context.Regexpr :=
           Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);

         Handle_Regexpr_Next_Value;
         Pop_Frame_Context;
      end if;
   end Handle_Regexpr_Next_Value;

   --------------------
   -- Handle_Regexpr --
   --------------------

   procedure Handle_Regexpr is

      procedure Control_Iteration;
      --  This procedure is to be called after the analysis of the current
      --  expression (left and right part), so when there's no more expression
      --  to analyze. It will control the overall iteration, stop it upon
      --  having found a result unless we're in a generator mode.

      Captured_Variable     : aliased Capture_Result_Type;
      Sub_Matcher           : aliased Regexpr_Matcher_Type;
      Next_Matcher          : aliased Regexpr_Matcher_Type;
      Root_Capture_Callback : Boolean         := False;
      Matcher               : Regexpr_Matcher := Top_Context.Regexpr;

      -----------------------
      -- Control_Iteration --
      -----------------------

      procedure Control_Iteration is
         Matcher : Regexpr_Matcher := Top_Context.Regexpr;
      begin
         if Top_Object /= Match_False then
            if Matcher.Overall_Yield_Callback /= null then
               Top_Context.Visit_Decision.all := Into;
            else
               Top_Context.Visit_Decision.all := Stop;
            end if;
         end if;
      end Control_Iteration;

      Expr   : T_Expr   := Matcher.Current_Expr;
      Object : W_Object := Top_Object;

   begin
      if Top_Context.Regexpr = null then
         Push_Match_True (Top_Object);

         if Matcher.Overall_Yield_Callback /= null then
            Matcher.Overall_Yield_Callback.all;
         end if;

         return;
      end if;

      Push_Frame_Context;
      Top_Context.Regexpr_Anchored := True;

      if Expr.Kind = Template_Reg_Expr_Anchor then
         --  We reached a right anchor. We should not have reached this stage.
         --  Push true inconditially. The outer call will then convert this
         --  into false. Note that we can't just push false here, otherwise the
         --  outer call could not be able to differenciate cases where false is
         --  received because an element has been found and cases where false
         --  is received because no element has been extracted (which would be
         --  correct).

         Push_Match_True (Top_Object);
      elsif Expr.Kind = Template_Reg_Expr then
         if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier
           and then Expr.Reg_Expr_Left.Max > 0
           and then Matcher.Quantifiers_Hit >= Expr.Reg_Expr_Left.Max
         then
            --  If we hit the max, executing the quantifier is always false.
            --  Stack false and leave the caller to call right expression.

            Push_Match_False;
         else
            if not Matcher.Capture_Installed
              and then not Expr.Node.As_Reg_Expr.F_Captured.Is_Null
            then
               --  Capture callbacks are only available for expression with
               --  subexpressions. In the case of quantifier, we need to track
               --  which call actually installed the capture callabck to be
               --  able to remove it once all the quantification is done.

               Root_Capture_Callback     := True;
               Matcher.Capture_Installed := True;
               Captured_Variable.Object  :=
                 new W_Regexpr_Result_Type'(Result => new W_Vector_Type);
               Captured_Variable.Parent := Matcher.Capturing;
               Matcher.Capturing        := Captured_Variable'Unchecked_Access;
               Top_Frame.Symbols.Include
                 (Expr.Node.As_Reg_Expr.F_Captured.Text,
                  W_Object (Captured_Variable.Object));
            end if;

            Push_Frame_Context;

            Sub_Matcher                   := Matcher.all;
            Sub_Matcher.Capture_Installed := False;

            if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier then
               --  In the case of a quantifier, the outer expression to try
               --  is the qualifier itself, the left one is the Quantifer_Expr
               --  value.

               Sub_Matcher.Current_Expr := Expr.Reg_Expr_Left.Quantifier_Expr;
               Sub_Matcher.Outer_Next_Expr := Matcher;
            else
               --  In the case of a regular sub expression, the outer
               --  expression to try is the right expression of the current
               --  expression, the left one is in the field Reg_Expr_Left.

               Sub_Matcher.Current_Expr    := Expr.Reg_Expr_Left;
               Sub_Matcher.Outer_Next_Expr :=
                 Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);
               Sub_Matcher.Outer_Next_Expr.Capture_Installed := False;
            end if;

            Top_Context.Regexpr := Sub_Matcher'Unchecked_Access;
            Matcher.Quantifiers_Hit       := Matcher.Quantifiers_Hit + 1;
            Handle_Regexpr;
            Matcher.Quantifiers_Hit := Matcher.Quantifiers_Hit - 1;
            Control_Iteration;

            Pop_Frame_Context;

            if Root_Capture_Callback then
               Matcher.Capturing         := Matcher.Capturing.Parent;
               Matcher.Capture_Installed := False;
            end if;
         end if;
      else
         if not Evaluate_Match (Expr) then
            Push_Match_False;
         else
            Capture_Value (Matcher.Capturing, Capture);

            Push_Frame_Context;
            Top_Context.Regexpr :=
              Get_Right_Expression_Matcher (Next_Matcher'Unchecked_Access);

            if Top_Context.Regexpr /= null then
               Top_Context.Yield_Callback := Handle_Regexpr'Access;
               Handle_Regexpr_Next_Value;
               Control_Iteration;

               --  TODO: This may not be enough in the context of an all ()
               --  generation as we'll also want to rollback when exploring
               --  other
               if Top_Object = Match_False then
                  Capture_Value (Matcher.Capturing, Rollback);
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

      Pop_Frame_Context;
   end Handle_Regexpr;

end Wrapping.Runtime.Matching;
