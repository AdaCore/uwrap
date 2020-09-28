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

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Runtime.Commands;    use Wrapping.Runtime.Commands;
with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;
with Wrapping.Runtime.Closures;    use Wrapping.Runtime.Closures;
with Wrapping.Runtime.Parameters;  use Wrapping.Runtime.Parameters;
with Wrapping.Runtime.Nodes;       use Wrapping.Runtime.Nodes;

package body Wrapping.Runtime.Objects is

   procedure Call_Insert
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   procedure Call_Include
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   procedure Call_Append
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   procedure Call_Get
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   -----------------
   -- Call_Insert --
   -----------------

   procedure Call_Insert
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      P1, P2 : W_Object;
   begin
      if Object.all in W_Map_Type then
         if Params.Length /= 2 then
            Error ("two parameters expected for insert");
         end if;

         P1 := Evaluate_Expression (Params.Element (1).Expr);
         P2 := Evaluate_Expression (Params.Element (2).Expr);

         W_Map (Object).A_Map.Insert (P1, P2);
      elsif Object.all in W_Set_Type'Class then
         if Params.Length /= 1 then
            Error ("one parameters expected for include");
         end if;

         P1 := Evaluate_Expression (Params.Element (1).Expr);

         W_Set (Object).A_Set.Insert (P1);
      end if;

      Push_Object (W_Object (Object));
   end Call_Insert;

   ------------------
   -- Call_Include --
   ------------------

   procedure Call_Include
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      P1, P2 : W_Object;
   begin
      if Object.all in W_Map_Type'Class then
         if Params.Length /= 2 then
            Error ("two parameters expected for include");
         end if;

         P1 := Evaluate_Expression (Params.Element (1).Expr);
         P2 := Evaluate_Expression (Params.Element (2).Expr);

         W_Map (Object).A_Map.Include (P1, P2);
      elsif Object.all in W_Set_Type'Class then
         if Params.Length /= 1 then
            Error ("one parameters expected for include");
         end if;

         P1 := Evaluate_Expression (Params.Element (1).Expr);

         W_Set (Object).A_Set.Include (P1);
      end if;

      Push_Object (W_Object (Object));
   end Call_Include;

   -----------------
   -- Call_Append --
   -----------------

   procedure Call_Append
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Val : W_Object;
   begin
      if Object.all in W_Vector_Type then
         if Params.Length /= 1 then
            Error ("one parameters expected for append");
         end if;

         Val := Evaluate_Expression (Params.Element (1).Expr);

         W_Vector (Object).A_Vector.Append (Val);
      end if;

      Push_Object (W_Object (Object));
   end Call_Append;

   --------------
   -- Call_Get --
   --------------

   procedure Call_Get
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Key   : W_Object;
      Index : Integer;
   begin
      if Params.Length /= 1 then
         Error ("get expects one parameter");
      end if;

      Key := Evaluate_Expression (Params.Element (1).Expr);

      if Object.all in W_Map_Type'Class then
         if W_Map (Object).A_Map.Contains (Key) then
            Push_Object (W_Map (Object).A_Map.Element (Key));
         elsif Top_Context.Match_Mode /= Match_None then
            Push_Match_False;
         else
            Error ("map doesn't contain element "
                   & Copy_String (Key.Write_String));
         end if;
      elsif Object.all in W_Set_Type'Class then
         if W_Set (Object).A_Set.Contains (Key) then
            Push_Object (Key);
         elsif Top_Context.Match_Mode /= Match_None then
            Push_Match_False;
         else
            Error ("set doesn't contain element "
                   & Copy_String (Key.Write_String));
         end if;
      elsif Object.all in W_Vector_Type'Class then
         if Key.Dereference.all not in W_Integer_Type'Class then
            Error ("expected integer index");
         end if;

         Index := W_Integer (Key).Value;

         if Index in
             W_Vector (Object).A_Vector.First_Index ..
                   W_Vector (Object).A_Vector.Last_Index
         then
            Push_Object (W_Vector (Object).A_Vector.Element (Index));
         elsif Top_Context.Match_Mode /= Match_None then
            Push_Match_False;
         else
            Error ("vector doesn't contain index " & Index'Wide_Wide_Image);
         end if;
      else
         Error ("object doesn't provide get function");
      end if;
   end Call_Get;

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Reference_Type; Params : T_Arg_Vectors.Vector)
   is
   begin
      An_Entity.Value.Push_Call_Result (Params);
   end Push_Call_Result;

   ---------------------------
   -- Match_With_Top_Object --
   ---------------------------

   overriding function Match_With_Top_Object
     (An_Entity : access W_Reference_Type) return Boolean
   is
   begin
      return Match_With_Top_Object (An_Entity.Value);
   end Match_With_Top_Object;

   --------------
   -- Traverse --
   --------------

   overriding function Traverse
     (An_Entity    : access W_Reference_Type;
      A_Mode       : Traverse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action)
      return Visit_Action
   is
   begin
      return An_Entity.Value.Traverse
        (A_Mode, Include_Self, Final_Result, Visitor);
   end Traverse;

   ------------------------------
   -- Evaluate_Bowse_Functions --
   ------------------------------

   overriding procedure Push_Traverse_Result
     (An_Entity        : access W_Reference_Type; A_Mode : Traverse_Mode;
      Match_Expression : T_Expr)
   is
   begin
      An_Entity.Value.Push_Traverse_Result
        (A_Mode => A_Mode, Match_Expression => Match_Expression);
   end Push_Traverse_Result;

   ---------------------
   -- Generate_Values --
   ---------------------

   overriding procedure Generate_Values
     (Object : access W_Reference_Type; Expr : T_Expr)
   is
   begin
      Object.Value.Generate_Values (Expr);
   end Generate_Values;

   -----------------------
   -- Is_Text_Container --
   -----------------------

   function Is_Text_Container (Container : W_Vector_Type) return Boolean is
   begin
      for I of Container.A_Vector loop
         if I.all in W_Vector_Type'Class then
            if not W_Vector (I).Is_Text_Container then
               return False;
            end if;
         elsif I.all not in W_Text_Expression_Type'Class then
            return False;
         end if;
      end loop;

      return True;
   end Is_Text_Container;

   ----------------
   -- Push_Value --
   ----------------

   overriding function Push_Value
     (An_Entity : access W_Vector_Type; Name : Text_Type) return Boolean
   is
      Call : Call_Access;
   begin
      if Name = "get" then
         Call := Call_Get'Access;
      elsif Name = "append" then
         Call := Call_Append'Access;
      end if;

      if Call /= null then
         Push_Object
           (W_Object'
              (new W_Intrinsic_Function_Type'
                 (Prefix => W_Object (An_Entity), Call => Call,
                  others => <>)));
         return True;
      else
         return False;
      end if;
   end Push_Value;

   ------------------
   -- Write_String --
   ------------------

   function Write_String (Object : W_Vector_Type) return Buffer_Slice is
      Result : Buffer_Slice := Get_Empty_Slice;
   begin
      for T of Object.A_Vector loop
         if T /= null then
            Result.Last := T.Write_String.Last;
         end if;
      end loop;

      return Result;
   end Write_String;

   ---------------------
   -- Generate_Values --
   ---------------------

   procedure Generate_Values (Object : access W_Vector_Type; Expr : T_Expr) is
      Last_Result : W_Object := Match_False;
   begin
      for E of Object.A_Vector loop
         if Process_Generated_Value (E, Expr) = Stop then
            return;
         end if;

         if Top_Object /= Match_False then
            Last_Result := Top_Object;
         end if;

         Pop_Object;
      end loop;

      Push_Object (Last_Result);
   end Generate_Values;

   ----------------
   -- Push_Value --
   ----------------

   overriding function Push_Value
     (An_Entity : access W_Set_Type; Name : Text_Type) return Boolean
   is
      Call : Call_Access;
   begin
      if Name = "insert" then
         Call := Call_Insert'Access;
      elsif Name = "include" then
         Call := Call_Include'Access;
      elsif Name = "get" then
         Call := Call_Get'Access;
      end if;

      if Call /= null then
         Push_Object
           (W_Object'
              (new W_Intrinsic_Function_Type'
                 (Prefix => W_Object (An_Entity), Call => Call,
                  others => <>)));
         return True;
      else
         return False;
      end if;
   end Push_Value;

   ---------------------
   -- Generate_Values --
   ---------------------

   procedure Generate_Values (Object : access W_Set_Type; Expr : T_Expr) is
      Last_Result : W_Object := Match_False;
   begin
      for E of Object.A_Set loop
         if Process_Generated_Value (E, Expr) = Stop then
            return;
         end if;

         if Top_Object /= Match_False then
            Last_Result := Top_Object;
         end if;

         Pop_Object;
      end loop;

      Push_Object (Last_Result);
   end Generate_Values;

   ----------------
   -- Push_Value --
   ----------------

   overriding function Push_Value
     (An_Entity : access W_Map_Type; Name : Text_Type) return Boolean
   is
      Call : Call_Access;
   begin
      if Name = "insert" then
         Call := Call_Insert'Access;
      elsif Name = "include" then
         Call := Call_Include'Access;
      elsif Name = "get" then
         Call := Call_Get'Access;
      end if;

      if Call /= null then
         Push_Object
           (W_Object'
              (new W_Intrinsic_Function_Type'
                 (Prefix => W_Object (An_Entity), Call => Call,
                  others => <>)));
         return True;
      else
         return False;
      end if;
   end Push_Value;

   ---------------------
   -- Generate_Values --
   ---------------------

   procedure Generate_Values (Object : access W_Map_Type; Expr : T_Expr) is
      Last_Result : W_Object := Match_False;
   begin
      for E of Object.A_Map loop
         if Process_Generated_Value (E, Expr) = Stop then
            return;
         end if;

         if Top_Object /= Match_False then
            Last_Result := Top_Object;
         end if;

         Pop_Object;
      end loop;

      Push_Object (Last_Result);
   end Generate_Values;

   ------------------
   -- Write_String --
   ------------------

   overriding function Write_String
     (Object : W_Integer_Type) return Buffer_Slice
   is
   begin
      return Write_String (Object.Value'Wide_Wide_Image);
   end Write_String;

   ------------------
   -- Write_String --
   ------------------

   overriding function Write_String
     (Object : W_String_Type) return Buffer_Slice is
   begin
      return Write_String (To_Text (Object.Value));
   end Write_String;

   --------
   -- Lt --
   --------

   overriding function Lt
     (Left : access W_String_Type; Right : access W_Object_Type'Class)
      return Boolean
   is
      L, R : Buffer_Slice;
   begin
      if Left = Right then
         return False;
      elsif Right.all not in W_String_Type'Class then
         return W_Text_Expression_Type (Left.all).Lt (Right);
      else
         Push_Buffer_Cursor;
         L := Left.Write_String;
         R := Right.Write_String;
         Pop_Buffer_Cursor;

         return Buffer.Str (L.First.Offset .. L.Last.Offset)
           < Buffer.Str (R.First.Offset .. R.Last.Offset);
      end if;
   end Lt;

   --------
   -- Eq --
   --------

   overriding function Eq
     (Left : access W_String_Type; Right : access W_Object_Type'Class)
      return Boolean
   is
      L, R : Buffer_Slice;
   begin
      if Left = Right then
         return True;
      elsif Right.all not in W_String_Type'Class then
         return W_Text_Expression_Type (Left.all).Eq (Right);
      else
         Push_Buffer_Cursor;
         L := Left.Write_String;
         R := Right.Write_String;
         Pop_Buffer_Cursor;

         return Buffer.Str (L.First.Offset .. L.Last.Offset)
           = Buffer.Str (R.First.Offset .. R.Last.Offset);
      end if;
   end Eq;

   -----------------
   -- To_W_String --
   -----------------

   function To_W_String (Str : Text_Type) return W_String is
   begin
      return new W_String_Type'(Value => To_Unbounded_Text (Str));
   end To_W_String;

   -----------------
   -- To_W_String --
   -----------------

   function To_W_String (Str : Unbounded_Text_Type) return W_String is
   begin
      return new W_String_Type'(Value => Str);
   end To_W_String;

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Text_Expression_Type; Params : T_Arg_Vectors.Vector)
   is
   begin
      if Params.Length = 0 then
         Push_Match_True (An_Entity);
      elsif Params.Length = 1 then
         Push_Frame_Context_Parameter_With_Match (W_Object (An_Entity));
         Evaluate_Expression (Params.Element (1).Expr);
         Pop_Frame_Context;
      else
         Error ("string comparison takes one argument");
      end if;
   end Push_Call_Result;

   ------------------
   -- Write_String --
   ------------------

   overriding function Write_String
     (Object : W_Regexp_Type) return Buffer_Slice is
   begin
      return Write_String (To_Text (Object.Value));
   end Write_String;

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

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Intrinsic_Function_Type;
      Params    : T_Arg_Vectors.Vector)
   is
   begin
      Push_Frame_Context_Parameter;
      An_Entity.Call (An_Entity.Prefix, Params);
      Pop_Frame_Context;
   end Push_Call_Result;

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Function_Type; Params : T_Arg_Vectors.Vector)
   is

      procedure Evaluate_Parameter
        (Name : Text_Type; Position : Integer; Value : T_Expr);

      procedure Result_Callback;

      Calling_Frame : Data_Frame;
      Temp_Symbols  : W_Object_Maps.Map;

      ------------------------
      -- Evaluate_Parameter --
      ------------------------

      procedure Evaluate_Parameter
        (Name : Text_Type; Position : Integer; Value : T_Expr)
      is
         Computed_Name : constant Text_Type :=
           (if Name = "" then
              An_Entity.A_Function.Arguments_Ordered.Element (Position)
                .Name_Node.Text
            else Name);
      begin
         Temp_Symbols.Insert (Computed_Name, Evaluate_Expression (Value));
      end Evaluate_Parameter;

      Last_Result : W_Object;

      ---------------------
      -- Result_Callback --
      ---------------------

      procedure Result_Callback is
         Visit_Decision : Visit_Action;
         Prev_Top : constant W_Object := Top_Object;
      begin
         --  When reaching a value to be picked on a function f, either:
         --  (1) the caller is not iterating over generated values, in which
         --      case we found the value, we can interrupt the above expansion
         --      if any.
         --  (2) the caller is an iterating over generated values, e.g.
         --      f ().all(). In this case, we restore temporarily frame parent
         --      frame (the frame of the caller) and execute the rest of the
         --      caller actions for the specific value picked, then go back
         --      fetching other values for the function.

         if Calling_Frame.Top_Context.Yield_Callback = null then
            Last_Result                 := Top_Object;
            Top_Frame.Interrupt_Program := True;
         else
            Push_Frame (Calling_Frame);

            Visit_Decision := Process_Generated_Value (Prev_Top, null);
            Last_Result := Top_Object;

            if Top_Context.Visit_Decision /= null then
               Top_Context.Visit_Decision.all := Visit_Decision;
            end if;

            Pop_Object;
            Pop_Frame;
         end if;
      end Result_Callback;

      Prev_It : constant W_Object := Get_Implicit_It;
   begin
      Process_Parameters (Params, Evaluate_Parameter'Access);

      Calling_Frame := Top_Frame;

      Push_Frame (An_Entity.A_Function);

      Push_Implicit_It (Prev_It);
      Top_Frame.Symbols.Move (Temp_Symbols);
      Top_Context.Function_Result_Callback :=
        Result_Callback'Unrestricted_Access;
      Top_Context.Yield_Callback := null;

      Handle_Command_Sequence (An_Entity.A_Function.Program.First_Element);

      Pop_Frame;

      if Last_Result /= null then
         Push_Object (Last_Result);
      else
         Push_Match_False;
      end if;
   end Push_Call_Result;

   ----------------
   -- Push_Value --
   ----------------

   overriding function Push_Value
     (An_Entity : access W_Static_Entity_Type; Name : Text_Type) return Boolean
   is
      A_Semantic_Entity : T_Entity;
   begin
      if An_Entity.An_Entity.Children_Indexed.Contains (Name) then
         A_Semantic_Entity :=
           An_Entity.An_Entity.Children_Indexed.Element (Name);

         if A_Semantic_Entity.all in T_Var_Type'Class then
            --  We found a reference to a Var. This means that we need to
            --  process the node corresponding to this module, and retreive
            --  the actual variable value.

            return
              Get_Object_For_Entity (An_Entity.An_Entity).Push_Value (Name);
         else
            Push_Object
              (W_Object'
                 (new W_Static_Entity_Type'(An_Entity => A_Semantic_Entity)));

            return True;
         end if;
      end if;

      return False;
   end Push_Value;

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Static_Entity_Type; Params : T_Arg_Vectors.Vector)
   is
      It_Object : W_Object;
      Result    : W_Object;
   begin
      --  Matching an static entity reference means two things:
      --    *  First, check that this entity reference exist in the context,
      --       that is it's of a subtype of the enclosing It.
      --    *  Second, check that the expression, if any, corresponds to the
      --       components of the enclosing It

      It_Object := Get_Implicit_It;

      --  This function currently only operates on template instances and
      --  template types. Check that It is a template of the right type.

      if It_Object.all not in W_Template_Instance_Type then
         Push_Match_False;
         return;
      end if;

      --  If we're of the right type, then push the implicit It so that the
      --  stack starts with an implicit entity at the top, and check the
      --  result.

      if Params.Length = 0 then
         Push_Match_True (It_Object);
         return;
      elsif Params.Length = 1 then
         --  Push_Frame_Context;
         Push_Implicit_It (It_Object);
         Evaluate_Expression (Params.Element (1).Expr);
         Result := Pop_Object;
         Pop_Object;
         --  Pop_Frame_Context;

         if Result /= Match_False then
            --  If the result is good, then the result of this match is the
            --  matched object.

            Push_Match_True (It_Object);
            return;
         else
            Push_Match_False;
            return;
         end if;
      else
         Error ("matching a static entity requires one parameter at most");
      end if;
   end Push_Call_Result;

   ---------------------
   -- Generate_Values --
   ---------------------

   overriding procedure Generate_Values
     (Object : access W_Static_Entity_Type; Expr : T_Expr)
   is
      A_Template : W_Template_Instance;
   begin
      if Object.An_Entity.all not in T_Template_Type'Class then
         Push_Match_False;
         return;
      end if;

      A_Template :=
        W_Template_Instance (Get_Object_For_Entity (Object.An_Entity));

      A_Template.Indexed_Variables.Element ("_registry").Generate_Values
        (Expr);
   end Generate_Values;

   ------------------
   -- Write_String --
   ------------------

   overriding function Write_String
     (Object : W_Deferred_Expr_Type) return Buffer_Slice
   is
      Result : Buffer_Slice;
   begin
      --  Evaluate the deferred expression and its string conversion in the
      --  context of the closure.

      Push_Frame (Object.A_Closure);
      Result := Evaluate_Expression (Object.Expr).Write_String;
      Pop_Frame;

      return Result;
   end Write_String;

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Deferred_Expr_Type;
      Params    : T_Arg_Vectors.Vector)
   is
      Result : W_Object;
   begin
      --  Evaluate the deferred expression in the context of the closure. The
      --  Actual params to this defer expression evaluation has to be done
      --  in the calling context. E.g in:
      --     d (v);
      --  d begin a deferred expression, while d needs to be evaluated in the
      --  closure, v refers to entities outside of the closure, in the inital
      --  context.

      Push_Frame (An_Entity.A_Closure);
      Evaluate_Expression (An_Entity.Expr);
      Result := Top_Object;
      Pop_Frame;

      Result.Push_Call_Result (Params);
   end Push_Call_Result;

   ---------------------
   -- Generate_Values --
   ---------------------

   overriding procedure Generate_Values
     (Object : access W_Regexpr_Result_Type; Expr : T_Expr)
   is
   begin
      Object.Result.Generate_Values (Expr);
   end Generate_Values;

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Regexpr_Result_Type; Params : T_Arg_Vectors.Vector)
   is
   begin
      An_Entity.As_Singleton.Push_Call_Result (Params);
   end Push_Call_Result;

   --------------
   -- Traverse --
   --------------

   overriding function Traverse
     (An_Entity  : access W_Regexpr_Result_Type; A_Mode : Traverse_Mode;
      Include_It : Boolean; Final_Result : out W_Object;
      Visitor    : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action)
      return Visit_Action
   is
   begin
      return
        An_Entity.As_Singleton.Traverse
          (A_Mode, Include_It, Final_Result, Visitor);
   end Traverse;

   ------------------------------
   -- Evaluate_Bowse_Functions --
   ------------------------------

   overriding procedure Push_Traverse_Result
     (An_Entity        : access W_Regexpr_Result_Type; A_Mode : Traverse_Mode;
      Match_Expression : T_Expr)
   is
   begin
      An_Entity.As_Singleton.Push_Traverse_Result
        (A_Mode, Match_Expression);
   end Push_Traverse_Result;

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

end Wrapping.Runtime.Objects;
