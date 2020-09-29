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
      Match_Expression : T_Expr := null) return Visit_Action
   is

      Visit_Decision : aliased Visit_Action := Unknown;

      Real_Generated : W_Object := W_Object (Generated);
      --  The actual generated value will be the one passed in parameter unless
      --  there's an allocation.

      Expression_Result : W_Object;

   begin
      --  First, process the match expression, or just run the outer action if
      --  there's no expression to process.

      if Match_Expression /= null then
         --  If there's a name capture above this expression, its value needs
         --  to be available in the underlying match expression. We only
         --  capture the entity outside of folding context. When folding, the
         --  result of the folding expression will actually be what needs to be
         --  captured.

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

         --  There is a subtetly in the browsing functions. The It reference
         --  within these calls isn't the entity currently analyzed anymore
         --  but directly the entity that is being evaluated under these calls.
         --  However, we cannot create a sub frame as whatever we match needs
         --  to find its way to the command frame (otherwise any extracted
         --  group would be deleted upon frame popped).
         Push_Implicit_It (Real_Generated);

         Evaluate_Expression (Match_Expression);

         Pop_Frame_Context;

         Expression_Result := Pop_Object;

         Pop_Object;

         if Expression_Result.all in W_Reference_Type'Class
           and then W_Reference (Expression_Result).Is_Allocated
         then
            --  If we have a result only in allocated mode, and if this result
            --  is a new entity, this means that this new entity is actually
            --  the result of the browse, not the one searched.

            Real_Generated := Expression_Result;
         end if;
      else
         --  There is no expression - we need to evaluate the outer callback
         --  explicitely, to cover cases such as e.g.:
         --     pick child().all(),
         --  child needs to be evaluated against the outer expression to be
         --  captured by the possible wrap or weave command.

         if Top_Context.Outer_Expr_Action /= Action_None then
            Push_Frame_Context;
            Push_Implicit_It (Real_Generated);

            Execute_Expr_Outer_Action;

            Pop_Object;
            Pop_Frame_Context;
         end if;
      end if;

      --  Then process the generated value and control the rest of the
      --  iteration.

      if Expression_Result = Match_False then
         --  The expression did not match. Return into to signal to the
         --  caller that another value needs to be generated.

         Push_Match_False;

         if Visit_Decision = Unknown then
            Visit_Decision := Into;
         end if;
      elsif Top_Context.Yield_Callback = null then
         --  We don't need to generate values, push the result and stop the
         --  search.

         Push_Object (Real_Generated);

         if Visit_Decision = Unknown then
            Visit_Decision := Stop;
         end if;
      else
         --  When evaluating a yield callback in a browsing call, we need to
         --  first deactivate yield in the expression itself. We also we need
         --  to remove potential name capture, as it would override the one we
         --  are capturing in this browsing iteration.

         Push_Frame_Context_Parameter;
         Top_Context.Name_Captured  := To_Unbounded_Text ("");
         Top_Context.Visit_Decision := Visit_Decision'Unchecked_Access;

         --  Then call yield

         Push_Implicit_It (Real_Generated);
         Call_Yield;

         --  Pop frame context. This will in particular restore the name
         --  catpure, which we're using as the accumulator.

         Pop_Frame_Context;

         --  If there's an name to store the result, store it there.

         if Top_Context.Name_Captured /= "" then
            Include_Symbol (To_Text (Top_Context.Name_Captured), Top_Object);
         end if;

         --  The result of the expansion can be calls to wrap functions,
         --  and one of this wrap function may be a visit decision. If
         --  that's the case, take it into account and reset the flag.

         if Visit_Decision = Unknown then
            Visit_Decision := Into;
         end if;
      end if;

      return Visit_Decision;
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
         --  If we're already on a Match_False, there's nothing to do. We're
         --  already not matching.

         Result := True;
      elsif Other_Entity.all in W_Regexp_Type'Class then
         --  If we're matching against a regular expression, retreive the
         --  string of that expression and the entity, and run the matcher.

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

         --  If we didn't match, replace the top object by false, otherwise
         --  keep it as-is.

         if not Matched then
            Pop_Object;
            Push_Match_False;
         end if;

         Pop_Buffer_Cursor;

         Result := True;
      elsif Other_Entity.all in W_Text_Expression_Type'Class then
         --  If we're matching against any kind of text other than regular
         --  expression, do a simple textual comparison.

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
      --  First compare tags of the object, and if they're the same, resolve
      --  to the address.

      if Left_Tag /= Right_Tag then
         return To_Address (Left_Tag) < To_Address (Right_Tag);
      else
         return Left.all'Address < Right.all'Address;
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
      --  Allocate a variable contained in the static entity in parameter

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
         --  This object has already been created, just return it.

         return Object_For_Entity_Registry.Element (Name);
      else
         --  We don't have the object yet. The static entity could be either
         --     - a module, in which case we're creating its global variables,
         --     - a template, in which case we're creating its reference
         --       registry.

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
