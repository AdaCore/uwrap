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

with Ada.Containers;        use Ada.Containers;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with Libtemplatelang.Common;     use Libtemplatelang.Common;

with Wrapping.Runtime.Structure;   use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Commands;    use Wrapping.Runtime.Commands;
with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Matching;    use Wrapping.Runtime.Matching;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;
with Wrapping.Runtime.Closures;    use Wrapping.Runtime.Closures;
with Wrapping.Runtime.Parameters;  use Wrapping.Runtime.Parameters;

package body Wrapping.Runtime.Objects is

   function Is_Wrapping (Node : access W_Node_Type'Class) return Boolean is
     (Node.all in W_Template_Instance_Type'Class
      and then W_Template_Instance (Node).Origin /= null);

   procedure Run_Deferred_Expr (Deferred_Expr : W_Deferred_Expr_Type);

   function Has_Allocator (Node : Template_Node'Class) return Boolean;

   procedure Call_Tmp
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   procedure Call_Insert
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   procedure Call_Include
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   procedure Call_Append
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   procedure Call_Get
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   -------------------
   -- Has_Allocator --
   -------------------

   function Has_Allocator (Node : Template_Node'Class) return Boolean is

      function Visit (Node : Template_Node'Class) return Visit_Status;

      Found : Boolean := False;

      -----------
      -- Visit --
      -----------

      function Visit (Node : Template_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Template_New_Expr then
            Found := True;
            return Stop;
         else
            return Into;
         end if;
      end Visit;
   begin
      Node.Traverse (Visit'Access);

      return Found;
   end Has_Allocator;

   generic
      A_Mode : Traverse_Mode;
   procedure Call_Gen_Browse
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   ---------------------
   -- Call_Gen_Browse --
   ---------------------

   procedure Call_Gen_Browse
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is

      procedure Generator (Expr : T_Expr);

      ---------------
      -- Generator --
      ---------------

      procedure Generator (Expr : T_Expr) is
      begin
         Push_Traverse_Result (Top_Object, A_Mode, Expr);
      end Generator;

   begin
      if Params.Length = 0 then
         Evaluate_Generator_Regexp
           (Object, null, Generator'Unrestricted_Access);
      elsif Params.Length = 1 then
         Evaluate_Generator_Regexp
           (Object, Params.Element (1).Expr, Generator'Unrestricted_Access);
      elsif Params.Length > 1 then
         Error ("matcher takes only 1 argument");
      end if;
   end Call_Gen_Browse;

   procedure Call_Browse_Parent is new Call_Gen_Browse (Parent);

   procedure Call_Browse_Child is new Call_Gen_Browse (Child_Breadth);

   procedure Call_Browse_Next is new Call_Gen_Browse (Next);

   procedure Call_Browse_Prev is new Call_Gen_Browse (Prev);

   procedure Call_Browse_Sibling is new Call_Gen_Browse (Sibling);

   procedure Call_Browse_Wrapper is new Call_Gen_Browse
     (Wrapping.Runtime.Structure.Wrapper);

   --------------
   -- Call_Tmp --
   --------------

   procedure Call_Tmp
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Slice : Buffer_Slice;
   begin
      if Params.Length = 0 then
         Push_Temporary_Name ("", W_Node (Object).Tmp_Counter);
      elsif Params.Length = 1 then
         Evaluate_Expression (Params.Element (1).Expr);
         Push_Buffer_Cursor;

         Slice := Pop_Object.Write_String;
         Push_Temporary_Name
           (Buffer.Str (Slice.First.Offset .. Slice.Last.Offset),
            W_Node (Object).Tmp_Counter);
         Pop_Buffer_Cursor;
      else
         Error ("tmp only accepts one argument");
      end if;
   end Call_Tmp;

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

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Vector_Type; Params : T_Arg_Vectors.Vector)
   is
      Result : W_Object;
   begin
      --  TODO: This will essentially enable checks against strings, which is
      --  useful when vector indeed represent strings. Verify if this is OK.
      --  We may need a specific vector string type for this, and have a more
      --  comprehensive test here.

      if Params.Length = 0 then
         Push_Match_True (An_Entity);
      elsif Params.Length = 1 then
         Push_Implicit_It (An_Entity);
         Evaluate_Expression (Params.Element (1).Expr);
         Result := Pop_Object;
         Pop_Object;

         if Result /= Match_False then
            Push_Match_True (An_Entity);
         else
            Push_Match_False;
         end if;
      end if;
   end Push_Call_Result;

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
      --  TODO: Should that be the high level call result?
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
      return Object.Value.Write_String;
   end Write_String;

   ------------------
   -- Write_String --
   ------------------

   overriding function Write_String
     (Object : W_Text_Conversion_Type) return Buffer_Slice
   is
   begin
      return Object.An_Object.Write_String;
   end Write_String;

   ------------------
   -- Write_String --
   ------------------

   overriding function Write_String
     (Object : W_Text_Vector_Type) return Buffer_Slice
   is
      Result : Buffer_Slice := Get_Empty_Slice;
   begin
      for T of Object.A_Vector loop
         Result.Last := T.Write_String.Last;
      end loop;

      return Result;
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
      Called_Frame  : Data_Frame;
      Temp_Symbols  : W_Object_Maps.Map;

      ------------------------
      -- Evaluate_Parameter --
      ------------------------

      procedure Evaluate_Parameter
        (Name : Text_Type; Position : Integer; Value : T_Expr)
      is
         Computed_Name : Text_Type :=
           (if Name = "" then
              An_Entity.A_Function.Arguments_Ordered.Element (Position)
                .Name_Node
                .Text
            else Name);
      begin
         Temp_Symbols.Insert (Computed_Name, Evaluate_Expression (Value));
      end Evaluate_Parameter;

      Last_Result : W_Object;

      ---------------------
      -- Result_Callback --
      ---------------------

      procedure Result_Callback is
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
            Top_Context.Visit_Decision.all :=
              Process_Generated_Value (Top_Object, null);

            Last_Result := Pop_Object;

            Pop_Frame;
         end if;
      end Result_Callback;

      Prev_It : W_Object := Get_Implicit_It;
   begin
      Handle_Call_Parameters (Params, Evaluate_Parameter'Access);

      Calling_Frame := Top_Frame;

      Push_Frame (An_Entity.A_Function);
      Called_Frame := Top_Frame;

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
      Prefix    : W_Template_Instance;
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

      Prefix := W_Template_Instance (It_Object);

      --  If we're of the right type, then push the implicit It so that the
      --  stack starts with an implicit entity at the top, and check the
      --  result.

      if Params.Length = 0 then
         Push_Match_True (It_Object);
         return;
      elsif Params.Length = 1 then
         Push_Implicit_It (It_Object);
         Evaluate_Expression (Params.Element (1).Expr);
         Result := Pop_Object;
         Pop_Object;

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
   begin
      Run_Deferred_Expr (Object);

      return Pop_Object.Write_String;
   end Write_String;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child (Parent, Child : access W_Node_Type'Class) is
   begin
      Child.Parent := W_Node (Parent);

      if Parent.Children_Ordered.Length > 0 then
         Parent.Children_Ordered.Last_Element.Next := W_Node (Child);
         Child.Prev := Parent.Children_Ordered.Last_Element;
      end if;

      Parent.Children_Ordered.Append (W_Node (Child));
   end Add_Child;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (Parent, Child : access W_Node_Type'Class; Name : Text_Type)
   is
   begin
      Add_Child (Parent, Child);
      Parent.Children_Indexed.Insert (Name, W_Node (Child));
   end Add_Child;

   --------------
   -- Add_Next --
   --------------

   procedure Add_Next (Cur, Next : access W_Node_Type'Class) is
      Found : Boolean := False with Ghost;
   begin
      Next.Next := Cur.Next;
      Next.Prev := W_Node (Cur);
      Cur.Next  := W_Node (Next);

      if Cur.Parent /= null then
         for I in
           Cur.Children_Ordered.First_Index .. Cur.Children_Ordered.Last_Index
         loop
            if Cur.Children_Ordered.Element (I) = Cur then
               Cur.Children_Ordered.Insert (I + 1, W_Node (Next));
               Found := True;
               exit;
            end if;
         end loop;

         pragma Assert (Found);
      end if;
   end Add_Next;

   ------------------------
   -- Add_Wrapping_Child --
   ------------------------

   procedure Add_Wrapping_Child (Parent, Child : access W_Node_Type'Class) is
      Wrapped : W_Node;
   begin
      if Is_Wrapping (Parent) then
         --  Template instances that are part of the wrapping tree are never
         --  added directly. Instead, they are wrapping a hollow node created
         --  on the origin tree.

         Wrapped := new W_Hollow_Node_Type;
         Add_Wrapping_Child (W_Template_Instance (Parent).Origin, Wrapped);
         Wrapped.Templates_Ordered.Append (W_Template_Instance (Child));
         W_Template_Instance (Child).Origin := W_Node (Wrapped);
      else
         Add_Child (Parent, Child);
      end if;
   end Add_Wrapping_Child;

   ------------------------------
   -- Create_Template_Instance --
   ------------------------------

   function Create_Template_Instance
     (An_Entity : access W_Node_Type'Class; A_Template : T_Template;
      Register  : Boolean) return W_Template_Instance
   is
      New_Template   : W_Template_Instance;
      Template_Class : W_Template_Instance;

      Current_Template : T_Template;
   begin
      New_Template                 := new W_Template_Instance_Type;
      New_Template.Defining_Entity := T_Entity (A_Template);

      if An_Entity /= null then
         New_Template.Origin := W_Node (An_Entity);

         if Register then
            An_Entity.Templates_By_Name.Insert
              (A_Template.Name_Node.Text, New_Template);
            An_Entity.Templates_By_Full_Id.Insert
              (A_Template.Full_Name, New_Template);
            An_Entity.Templates_Ordered.Append (New_Template);
         end if;
      end if;

      Current_Template := A_Template;

      while Current_Template /= null loop
         Template_Class :=
           W_Template_Instance (Get_Object_For_Entity (A_Template));

         if Register then
            W_Vector
              (W_Reference
                 (Template_Class.Indexed_Variables.Element ("_registry"))
                 .Value)
              .A_Vector
              .Append
              (W_Object (New_Template));
         end if;

         Current_Template := Current_Template.Extends;
      end loop;

      if Register then
         Register_Template_Instance (New_Template);
      end if;

      return New_Template;
   end Create_Template_Instance;

   ---------------------------
   -- Get_Template_Instance --
   ---------------------------

   function Get_Template_Instance
     (An_Entity : access W_Node_Type'Class; Name : Text_Type)
      return W_Template_Instance
   is
   begin
      if An_Entity.Templates_By_Name.Contains (Name) then
         return An_Entity.Templates_By_Name.Element (Name);
      else
         return null;
      end if;
   end Get_Template_Instance;

   ---------------------------
   -- Get_Template_Instance --
   ---------------------------

   function Get_Template_Instance
     (An_Entity : access W_Node_Type'Class; A_Template : T_Template)
      return W_Template_Instance
   is
   begin
      --  TODO: These calls to full name may be very costly, it'd be better to
      --  cache the full name in the object
      if An_Entity.Templates_By_Full_Id.Contains (A_Template.Full_Name) then
         return An_Entity.Templates_By_Full_Id.Element (A_Template.Full_Name);
      else
         return null;
      end if;
   end Get_Template_Instance;

   ----------------
   -- Push_Value --
   ----------------

   overriding function Push_Value
     (An_Entity : access W_Node_Type; Name : Text_Type) return Boolean
   is
      A_Call       : Call_Access := null;
      Is_Generator : Boolean     := False;
   begin
      if An_Entity.Templates_By_Name.Contains (Name) then
         Push_Object (An_Entity.Templates_By_Name.Element (Name));

         return True;
      elsif Name = "parent" then
         A_Call       := Call_Browse_Parent'Access;
         Is_Generator := True;
      elsif Name = "child" then
         A_Call       := Call_Browse_Child'Access;
         Is_Generator := True;
      elsif Name = "next" then
         A_Call       := Call_Browse_Next'Access;
         Is_Generator := True;
      elsif Name = "prev" then
         A_Call       := Call_Browse_Prev'Access;
         Is_Generator := True;
      elsif Name = "sibling" then
         A_Call       := Call_Browse_Sibling'Access;
         Is_Generator := True;
      elsif Name = "wrapper" then
         A_Call       := Call_Browse_Wrapper'Access;
         Is_Generator := True;
      elsif Name = "tmp" then
         A_Call := Call_Tmp'Access;
      end if;

      if A_Call /= null then
         Push_Object
           (W_Object'
              (new W_Intrinsic_Function_Type'
                 (Prefix    => W_Object (An_Entity), Call => A_Call,
                  Generator => Is_Generator)));

         return True;
      end if;

      if Name = "language" then
         Push_Object (To_W_String (W_Node (An_Entity).Language));

         return True;
      end if;

      return False;
   end Push_Value;

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Node_Type; Params : T_Arg_Vectors.Vector)
   is
   begin
      --  TODO: this code is probably the generic call result code, not
      --  specific to
      --   node type.
      if Params.Length = 0 then
         Push_Match_True (An_Entity);
      elsif Params.Length = 1 then
         Push_Implicit_It (W_Object (An_Entity));
         Push_Match_Result (Params.Element (1).Expr, W_Object (An_Entity));
         Delete_Object_At_Position (-2);
      else
         Error ("comparing with a node requires one parameter");
      end if;
   end Push_Call_Result;

   ---------------------------
   -- Match_With_Top_Object --
   ---------------------------

   function Match_With_Top_Object
     (An_Entity : access W_Node_Type) return Boolean
   is
      Other_Entity : W_Object := Top_Object.Dereference;
   begin
      --  By default, nodes only consider ref as being "is" matches, and calls
      --  as being "has" matches. So pass through calls before looking.

      if Top_Context.Match_Mode = Match_Call_Default then
         return True;
      end if;

      --  If the two entities are equal, match true

      if An_Entity = Other_Entity then
         return True;
      end if;

      --  Otherwise, see if the main checker finds something.

      if W_Object_Type (An_Entity.all).Match_With_Top_Object then
         return True;
      end if;

      return False;
   end Match_With_Top_Object;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (An_Entity  : access W_Node_Type; A_Mode : Traverse_Mode;
      Include_It : Boolean; Final_Result : out W_Object;
      Visitor    : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action)
      return Visit_Action
   is
      function Traverse_Wrapper
        (Entity : access W_Object_Type'Class; A_Mode : Traverse_Mode)
         return Visit_Action;
      --  Wraps the default traverse function, capturing the result if not null
      --  or false.

      function Visit_Wrapper
        (Entity : access W_Object_Type'Class) return Visit_Action;
      --  Wraps the default visit function, capturing the result if not null or
      --  false.

      Current               : W_Node;
      Current_Children_List : W_Node_Vectors.Vector;
      Next_Children_List    : W_Node_Vectors.Vector;

      ----------------------
      -- Traverse_Wrapper --
      ----------------------

      function Traverse_Wrapper
        (Entity : access W_Object_Type'Class; A_Mode : Traverse_Mode)
         return Visit_Action
      is
         Temp_Result : W_Object;
         R           : Visit_Action;
      begin
         R := Entity.Traverse (A_Mode, False, Temp_Result, Visitor);

         if Temp_Result /= Match_False and then Temp_Result /= null then
            Final_Result := Temp_Result;
         end if;

         return R;
      end Traverse_Wrapper;

      -------------------
      -- Visit_Wrapper --
      -------------------

      function Visit_Wrapper
        (Entity : access W_Object_Type'Class) return Visit_Action
      is
         Temp_Result : W_Object;
         R           : Visit_Action;
      begin
         R := Visitor (Entity, Temp_Result);

         if Temp_Result /= Match_False and then Temp_Result /= null then
            Final_Result := Temp_Result;
         end if;

         return R;
      end Visit_Wrapper;

      Decision : Visit_Action := Unknown;

   begin
      Final_Result := Match_False;

      W_Node_Type'Class (An_Entity.all).Pre_Visit;

      if Include_It then
         W_Node_Type'Class (An_Entity.all).Pre_Visit;

         case Visit_Wrapper (An_Entity) is
            when Stop =>
               return Stop;

            when Over =>
               return Over;

            when Into | Into_Override_Anchor =>
               null;

            when Unknown =>
               null;
         end case;
      end if;

      if A_Mode = Sibling then
         --  TODO: Rewrite sibling as a next starting from the first element.
         --  the current version doesn't handle properly regular expressions
         case Traverse_Wrapper (An_Entity, Prev) is
            when Stop =>
               return Stop;

            when Over =>
               return Over;

            when Into | Into_Override_Anchor =>
               null;

            when Unknown =>
               null;
         end case;

         return Traverse_Wrapper (An_Entity, Next);
      elsif A_Mode = Wrapper then
         for T of An_Entity.Templates_Ordered loop
            W_Node_Type'Class (T.all).Pre_Visit;

            case Visit_Wrapper (T) is
               when Over =>
                  null;

               when Stop =>
                  return Stop;

               when Into | Into_Override_Anchor =>
                  null;

               when Unknown =>
                  null;
            end case;
         end loop;

         return Into;
      end if;

      case A_Mode is
         when Parent =>
            Current := An_Entity.Parent;

         when Next =>
            Current := An_Entity.Next;

         when Prev =>
            Current := An_Entity.Prev;

         when Child_Depth =>
            if An_Entity.Children_Ordered.Length > 0 then
               Current := An_Entity.Children_Ordered.First_Element;
            end if;

         when Child_Breadth =>
            for C of An_Entity.Children_Ordered loop
               Current_Children_List.Append (C);
            end loop;

         when Sibling | Wrapper =>
            null;

      end case;

      if A_Mode = Child_Breadth then
         loop
            for C of Current_Children_List loop
               W_Node_Type'Class (C.all).Pre_Visit;

               Decision := Visit_Wrapper (C);

               case Decision is
                  when Stop =>
                     return Stop;

                  when Over =>
                     null;

                  when Into | Into_Override_Anchor =>
                     if Decision = Into_Override_Anchor
                       or else not Top_Context.Regexpr_Anchored
                     then
                        for C2 of C.Children_Ordered loop
                           Next_Children_List.Append (C2);
                        end loop;
                     end if;

                  when Unknown =>
                     null;
               end case;
            end loop;

            exit when Next_Children_List.Length = 0;

            Current_Children_List.Clear;
            Current_Children_List.Assign (Next_Children_List);
            Next_Children_List.Clear;
         end loop;
      else
         while Current /= null loop
            W_Node_Type'Class (Current.all).Pre_Visit;

            Decision := Visit_Wrapper (Current);

            case Decision is
               when Stop =>
                  return Stop;

               when Over =>
                  null;

               when Into | Into_Override_Anchor =>
                  if A_Mode = Child_Depth then
                     Decision := Traverse_Wrapper (Current, A_Mode);

                     case Decision is
                        when Stop =>
                           return Stop;

                        when others =>
                           null;
                     end case;
                  end if;

               when Unknown =>
                  null;
            end case;

            if Top_Context.Regexpr_Anchored
              and then Decision /= Into_Override_Anchor
            then
               return Stop;
            end if;

            case A_Mode is
               when Parent =>
                  Current := Current.Parent;

               when Prev =>
                  Current := Current.Prev;

               when Next | Child_Depth =>
                  Current := Current.Next;

               when Child_Breadth =>
                  null;

               when Sibling | Wrapper =>
                  null;

            end case;
         end loop;
      end if;

      return Into;
   end Traverse;

   --------------------------
   -- Push_Traverse_Result --
   --------------------------

   procedure Push_Traverse_Result
     (An_Entity        : access W_Node_Type; A_Mode : Traverse_Mode;
      Match_Expression : T_Expr)
   is

      function Visitor
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action;

      function Create_Hollow_Next
        (Prev : access W_Node_Type'Class) return W_Hollow_Node;

      procedure Allocate (E : access W_Object_Type'Class);

      -------------
      -- Visitor --
      -------------

      function Visitor
        (E : access W_Object_Type'Class;
         Result : out W_Object)
         return Visit_Action
      is
         Action : Visit_Action;
      begin
         Action := Process_Generated_Value (E, Match_Expression);
         Result := Pop_Object;

         return Action;
      end Visitor;

      ------------------------
      -- Create_Hollow_Next --
      ------------------------

      function Create_Hollow_Next
        (Prev : access W_Node_Type'Class) return W_Hollow_Node
      is
         Wrapped  : W_Hollow_Node;
         New_Node : W_Hollow_Node := new W_Hollow_Node_Type;
      begin
         if Is_Wrapping (Prev) then
            Wrapped := Create_Hollow_Next (W_Template_Instance (Prev).Origin);
            Wrapped.Templates_Ordered.Append (W_Template_Instance (New_Node));
            New_Node.Origin := W_Node (Wrapped);
         else
            --  TODO: What if parent is null?
            Add_Child (Prev.Parent, New_Node);
         end if;

         return New_Node;
      end Create_Hollow_Next;

      --------------
      -- Allocate --
      --------------

      procedure Allocate (E : access W_Object_Type'Class) is
      begin
         --  TODO: We need to be able to cancel allocation if the entire
         --  research happens to be false

         case A_Mode is
            when Child_Depth | Child_Breadth =>
               Add_Wrapping_Child (An_Entity, W_Node_Type (E.all)'Access);

            when others =>
               Error ("allocation not implemented on the enclosing function");
         end case;
      end Allocate;

      Found  : Boolean;
      Result : W_Object;
   begin
      Push_Frame_Context;
      Top_Context.Allocate_Callback := null;

      Found :=
        W_Node_Type'Class (An_Entity.all).Traverse
          (A_Mode, False, Result, Visitor'Access) =
        Stop;

      if not Found and then Match_Expression /= null
        and then Match_Expression.Has_New
      then
         --  Semantic for search is to look first for matches that do
         --  not require an allocator. If none is found and if there
         --  are allocators, then re-try, this time with allocators enabled.

         if Top_Context.Yield_Callback /= null then
            --  TODO: it would be best to check that earlier in the system, as
            --  opposed to only when trying to call a folding function.
            Error ("allocators are not allowed in folding browsing functions");
         end if;

         Top_Context.Allocate_Callback := Allocate'Unrestricted_Access;

         Found :=
           W_Node_Type'Class (An_Entity.all).Traverse
             (A_Mode, False, Result, Visitor'Access) =
           Stop;

         if not Found then
            --  If still not found, there is still a possibilty that this
            --  can match without any object valid, and then create the
            --  first element.

            declare
               Dummy_Entity : W_Node;
            begin
               Dummy_Entity := new W_Node_Type;

               Found := Visitor (Dummy_Entity, Result) = Stop;
            end;
         end if;
      end if;

      Pop_Frame_Context;
      Push_Frame_Context;
      Top_Context.Match_Mode := Match_Ref_Default;

      if not Found
        and then not
          (Top_Context.Match_Mode /= Match_None
           or else Top_Context.Yield_Callback /= null)
      then
         Error ("no result found for browsing function");
      end if;

      Pop_Frame_Context;

      if Result /= null then
         Push_Object (Result);
      else
         Push_Match_False;
      end if;
   end Push_Traverse_Result;

   -----------
   -- Print --
   -----------

   procedure Print (An_Entity : W_Node_Type; Indent : Text_Type := "") is
   begin
      Push_Buffer_Cursor;
      Put_Line (Indent & Copy_String (An_Entity.Write_String));
      Pop_Buffer_Cursor;

      for E of An_Entity.Children_Ordered loop
         Print (E.all, Indent & "-");
      end loop;
   end Print;

   ----------------
   -- Push_Value --
   ----------------

   function Push_Value
     (An_Entity : access W_Template_Instance_Type; Name : Text_Type)
      return Boolean
   is
      use Wrapping.Semantic.Structure;
   begin
      if W_Node_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      --  First cover the case of a variable or a pattern

      if Name = "origin" then
         if An_Entity.Origin /= null then
            Push_Object (An_Entity.Origin);

            return True;
         end if;
      elsif Name = "kind" then
         Push_Object (To_W_String (An_Entity.Defining_Entity.Full_Name));

         return True;
      elsif An_Entity.Indexed_Variables.Contains (Name) then
         Push_Object (An_Entity.Indexed_Variables.Element (Name));
         return True;
      end if;

      return False;
   end Push_Value;

   ---------------------------
   -- Match_With_Top_Object --
   ---------------------------

   overriding function Match_With_Top_Object
     (An_Entity : access W_Template_Instance_Type) return Boolean
   is
      Other_Entity : W_Object := Top_Object.Dereference;
   begin
      --  Special treatment for static entities, that are always checked in
      --  "is" mode

      if Other_Entity.all in W_Static_Entity_Type'Class then
         if Top_Context.Match_Mode in Match_Call_Default |
               Match_Ref_Default                                   | Match_Is
         then
            if not Instance_Of
                (T_Template (An_Entity.Defining_Entity),
                 T_Template (W_Static_Entity (Other_Entity).An_Entity))
            then
               Pop_Object;
               Push_Match_False;
            end if;
         else
            Error ("match has not available for static entity");
         end if;

         return True;
      end if;

      if W_Node_Type (An_Entity.all).Match_With_Top_Object then
         return True;
      end if;

      return False;
   end Match_With_Top_Object;

   --------------
   -- Traverse --
   --------------

   overriding function Traverse
     (An_Entity  : access W_Template_Instance_Type; A_Mode : Traverse_Mode;
      Include_It : Boolean; Final_Result : out W_Object;
      Visitor    : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action)
      return Visit_Action
   is

      function Template_Visitor
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action;

      ----------------------
      -- Template_Visitor --
      ----------------------

      function Template_Visitor
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action
      is
         Current_Result : W_Object;
         Last_Decision  : Visit_Action := Into;
      begin
         Result := Match_False;

         if E.all in W_Node_Type'Class then
            if W_Node (E).Templates_Ordered.Length = 0 then
               --  When there's no template for a given node, we consider this
               --  note to be non-existent from the template browsing point
               --  of view. As a result, anchored browsing should be allowed
               --  to look at the next level of nodes as if it was directly
               --  adjacent.

               return Into_Override_Anchor;
            else
               Push_Frame_Context;
               Top_Context.Is_First_Matching_Wrapper := True;

               for T of W_Node (E).Templates_Ordered loop
                  Last_Decision := Visitor (T, Current_Result);

                  if Current_Result /= Match_False
                    and then Current_Result /= null
                  then
                     Top_Context.Is_First_Matching_Wrapper := False;
                     Result := Current_Result;
                  end if;

                  exit when Last_Decision = Stop;
               end loop;

               Pop_Frame_Context;
            end if;
         end if;

         return Last_Decision;
      end Template_Visitor;

      Last_Decision : Visit_Action := Into;
      Result        : W_Object;
   begin
      Result := Match_False;

      --  A template instance either belong to an input tree (if has been
      --  created through a new from an input node) or a wrapping tree (in
      --  all other cases). If it doesn't have an origin set, it's part of the
      --  input tree, in this case fallback to the normal traversal. Otherwise,
      --  use specialized traversing using the original tree as the backbone of
      --  the iteration.
      if An_Entity.Origin = null then
         Last_Decision :=
           W_Node_Type (An_Entity.all).Traverse
             (A_Mode, Include_It, Result, Visitor);
      else
         Last_Decision :=
           An_Entity.Origin.Traverse
             (A_Mode, False, Result, Template_Visitor'Access);
      end if;

      if Result /= null and then Result /= Match_False then
         Final_Result := Result;
      end if;

      return Last_Decision;
   end Traverse;

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

end Wrapping.Runtime.Objects;
