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

with System;                          use System;

with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Tags;                          use Ada.Tags;
with Ada.Characters.Conversions;        use Ada.Characters.Conversions;
with Ada.Unchecked_Conversion;

with Libtemplatelang.Analysis;   use Libtemplatelang.Analysis;

with Wrapping.Runtime.Objects;     use Wrapping.Runtime.Objects;
with Wrapping.Runtime.Nodes;       use Wrapping.Runtime.Nodes;
with Wrapping.Runtime.Strings;     use Wrapping.Runtime.Strings;
with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;
with Wrapping.Runtime.Matching;    use Wrapping.Runtime.Matching;

package body Wrapping.Runtime.Structure is

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

   --------------
   -- Traverse --
   --------------

   function Traverse
     (An_Entity  : access W_Object_Type; A_Mode : Traverse_Mode;
      Include_Self : Boolean; Final_Result : out W_Object;
      Visitor    : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action)
      return Visit_Action
   is
   begin
      Final_Result := null;
      return Into;
   end Traverse;

   ------------------------------
   -- Evaluate_Bowse_Functions --
   ------------------------------

   procedure Push_Traverse_Result
     (An_Entity        : access W_Object_Type;
      A_Mode           : Traverse_Mode;
      Match_Expression : T_Expr) is
   begin
      Push_Match_False;
   end Push_Traverse_Result;

   -------------------
   -- Browse_Entity --
   -------------------

   function Process_Generated_Value
     (Generated        : access W_Object_Type'Class;
      Match_Expression : T_Expr) return Visit_Action
   is

      procedure Push_Yield_Result with
         Post => W_Stack_Size = W_Stack_Size'Old + 1;

      Visit_Decision : aliased Visit_Action := Unknown;

      Real_Generated : W_Object := W_Object (Generated);
      --  The actual generated value will be the one passed in parameter unless
      --  there's an allocation.

      -----------------------------
      -- Evaluate_Yield_Function --
      -----------------------------

      procedure Push_Yield_Result is
      begin
         --  In certain cases, there's no expression to be evaluated upon
         --  yield. E.g.:
         --    x.all ()
         --  as opposed to:
         --    x.all().something().
         if Top_Context.Yield_Callback = null then
            Push_Object (Real_Generated);

            return;
         end if;

         --  When evaluating a yield callback in a browsing call, we need to
         --  first deactivate yield in the expression itself. We also we need
         --  to remove potential name capture, as it would override the one we
         --  are capturing in this browsing iteration. TODO: quite the opposite
         --  if we do fold (i : inti, i: acc);

         Push_Frame_Context_Parameter;
         Top_Context.Name_Captured     := To_Unbounded_Text ("");
         Top_Context.Outer_Expr_Action := Action_Match;
         Top_Context.Visit_Decision    := Visit_Decision'Unchecked_Access;

         --  Then evaluate that folding expression

         Push_Implicit_It (Real_Generated);
         Push_Object (Real_Generated);
         Call_Yield;
         Pop_Underneath_Top;

         --  Pop frame context. This will in particular restore the name
         --  catpure, which we're using as the accumulator.
         Pop_Frame_Context;

         --  If there's an name to store the result, store it there.

         if Top_Context.Name_Captured /= "" then
            Include_Symbol (To_Text (Top_Context.Name_Captured), Top_Object);
         end if;
      end Push_Yield_Result;

      Expression_Result : W_Object;

   begin
      --  If the match expression is null, we're only looking for the presence
      --  of a node, not its form. The result is always true.

      if Match_Expression = null then
         --  In the case of
         --     pick child().all(),
         --  child needs to be evaluated against the outer expression to be
         --  captured by the possible wrap or weave command.

         if Top_Context.Outer_Expr_Action /= Action_None then
            Push_Frame_Context;
            Top_Context.Visit_Decision := Visit_Decision'Unchecked_Access;
            Push_Implicit_It (Real_Generated);

            Execute_Expr_Outer_Action;

            Pop_Object;
            Pop_Frame_Context;
         end if;

         if Top_Context.Yield_Callback /= null then
            Push_Yield_Result;

            if Visit_Decision = Unknown then
               return Into;
            else
               return Visit_Decision;
            end if;
         else
            Push_Object (Real_Generated);

            return Stop;
         end if;
      end if;

      --  There is a subtetly in the browsing functions. The It reference
      --  within these calls isn't the entity currently analyzed anymore
      --  but directly the entity that is being evaluated under these calls.
      --  However, we cannot create a sub frame as whatever we match needs
      --  to find its way to the command frame (otherwise any extracted group
      --  would be deleted upon frame popped).
      Push_Implicit_It (Real_Generated);

      --  If there's a name capture above this expression, its value needs to
      --  be available in the underlying match expression. We only capture the
      --  entity outside of folding context. When folding, the result of the
      --  folding expression will actually be what needs to be captured.

      if not (Top_Context.Name_Captured = "")
        and then Top_Context.Yield_Callback = null
      then
         Include_Symbol
           (To_Text (Top_Context.Name_Captured), Real_Generated);
      end if;

      --  Prior to evaluating the expression, we need to remove potential
      --  name capture, as it would override the one we are capturing in
      --  this browsing iteration.

      Push_Frame_Context_Parameter_With_Match (Real_Generated);
      Top_Context.Name_Captured  := To_Unbounded_Text ("");
      Top_Context.Visit_Decision := Visit_Decision'Unchecked_Access;
      Top_Context.Yield_Callback := null;

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

            Real_Generated := Expression_Result;
         end if;

         if Top_Context.Yield_Callback /= null then
            Push_Yield_Result;

            --  The result of the expansion can be calls to wrap functions,
            --  and one of this wrap function may be a visit decision. If
            --  that's the case, take it into account and reset the flag.

            if Visit_Decision = Unknown then
               return Into;
            else
               return Visit_Decision;
            end if;
         else
            Push_Object (Real_Generated);

            return Stop;
         end if;
      else
         Push_Match_False;

         return Into;
      end if;
   end Process_Generated_Value;

   ------------------
   -- Write_String --
   ------------------

   function Write_String
     (Object : W_Object_Type) return Buffer_Slice is
   begin
      return Get_Empty_Slice;
   end Write_String;

   --------------------
   -- Include_Symbol --
   --------------------

   procedure Include_Symbol (Name : Text_Type; Object : not null W_Object) is
   begin
      pragma Assert
        (if Object.all in W_Reference_Type'Class then
           W_Reference (Object).Value /= null);

      Top_Frame.Symbols.Include (Name, Object);
   end Include_Symbol;

   ----------------
   -- Stack_Size --
   ----------------

   function W_Stack_Size return Natural is
   begin
      return Natural (Top_Frame.Data_Stack.Length);
   end W_Stack_Size;

   ----------------
   -- Push_Value --
   ----------------

   function Push_Value
     (An_Entity : access W_Object_Type; Name : Text_Type) return Boolean is
   begin
      return False;
   end Push_Value;

   ----------------------
   -- Push_Call_Result --
   ----------------------

   procedure Push_Call_Result
     (An_Entity : access W_Object_Type; Params : T_Arg_Vectors.Vector)
   is
   begin
      Error
        ("non callable entity " &
         To_Wide_Wide_String
           (Ada.Tags.External_Tag (W_Object_Type'Class (An_Entity.all)'Tag)));
   end Push_Call_Result;

   ---------------------
   -- Generate_Values --
   ---------------------

   procedure Generate_Values (Object : access W_Object_Type; Expr : T_Expr) is
   begin
      Push_Match_Result (Expr, W_Object (Object));
      Call_Yield;
   end Generate_Values;

   ---------------------------
   -- Match_With_Top_Object --
   ---------------------------

   function Match_With_Top_Object
     (An_Entity : access W_Object_Type) return Boolean
   is
      Other_Entity : constant W_Object := Top_Object.Dereference;
      Matched      : Boolean;
      Result       : Boolean := False;
   begin
      Push_Frame_Context;

      if Other_Entity = Match_False then
         Result := True;
      elsif Other_Entity.all in W_Regexp_Type'Class then
         Push_Buffer_Cursor;

         declare
            L : constant Buffer_Slice := Other_Entity.Write_String;
            R : constant Buffer_Slice :=
              W_Object_Type'Class (An_Entity.all).Write_String;
         begin
            Matched :=
              Runtime.Matching.Match
                (Buffer.Str (L.First.Offset .. L.Last.Offset),
                 Buffer.Str (R.First.Offset .. R.Last.Offset));
         end;

         if not Matched then
            Pop_Object;
            Push_Match_False;
         end if;

         Pop_Buffer_Cursor;

         Result := True;
      elsif Other_Entity.all in W_Text_Expression_Type'Class then
         Push_Buffer_Cursor;

         declare
            L : constant Buffer_Slice := Other_Entity.Write_String;
            R : constant Buffer_Slice :=
              W_Object_Type'Class (An_Entity.all).Write_String;
         begin
            if Buffer.Str (L.First.Offset .. L.Last.Offset) /=
              Buffer.Str (R.First.Offset .. R.Last.Offset)
            then
               Pop_Object;
               Push_Match_False;
            end if;
         end;

         Pop_Buffer_Cursor;

         Result := True;
      elsif Other_Entity.all in W_Intrinsic_Function_Type'Class then
         --  Functions always match, their result is evaluated later.

         Result := True;
      end if;

      Pop_Frame_Context;

      return Result;
   end Match_With_Top_Object;

   --------
   -- Lt --
   --------

   function Lt
     (Left : access W_Object_Type; Right : access W_Object_Type'Class)
      return Boolean
   is
      Left_Tag  : constant Tag := W_Object (Left).all'Tag;
      Right_Tag : constant Tag := Right.all'Tag;

      function To_Address is new Ada.Unchecked_Conversion
        (Tag, System.Address);
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

   function Eq
     (Left : access W_Object_Type; Right : access W_Object_Type'Class)
      return Boolean
   is
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

      procedure Allocate_Variable (Var : T_Var);

      Result : W_Template_Instance;
      Name   : constant Text_Type := An_Entity.Full_Name;

      -----------------------
      -- Allocate_Variable --
      -----------------------

      procedure Allocate_Variable (Var : T_Var) is
      begin
         case Var.Kind is
            when Map_Kind =>
               Result.Indexed_Variables.Insert
                 (Var.Name_Node.Text,
                  new W_Reference_Type'
                    (Value => new W_Map_Type, others => <>));

            when Text_Kind =>
               Result.Indexed_Variables.Insert
                 (Var.Name_Node.Text,
                  new W_Reference_Type'
                    (Value => new W_String_Type, others => <>));

            when others =>
               Error ("global variable type not yet supported");

         end case;
      end Allocate_Variable;

   begin
      if Object_For_Entity_Registry.Contains (Name) then
         return Object_For_Entity_Registry.Element (Name);
      else
         Result                 := new W_Template_Instance_Type;
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
               new W_Reference_Type'
                 (Value => new W_Vector_Type, others => <>));
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

end Wrapping.Runtime.Structure;
