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

with Wrapping.Run;              use Wrapping.Run;
use Wrapping.Run.Adalang.Input;
with Wrapping.Semantic.Analysis;   use Wrapping.Semantic.Analysis;
with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Objects;     use Wrapping.Runtime.Objects;
with Wrapping.Runtime.Matching;    use Wrapping.Runtime.Matching;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;
with Wrapping.Runtime.Strings;     use Wrapping.Runtime.Strings;

package body Wrapping.Input.Ada is

   type Actual_To_Formal_Assoc is record
      Actual_Expr  : T_Expr;
      Formal_Index : Integer;
      Formal_Type  : Type_Constraint;
   end record;

   type Actual_To_Formal_Assoc_Array is array
     (Positive range <>) of Actual_To_Formal_Assoc;

   function Match_Params
     (Profile : Any_Member_Reference; Args : T_Arg_Vectors.Vector)
      return Actual_To_Formal_Assoc_Array;
   --  Match parameters in the profile against the arg in parameters, and set
   --  the corresponding actual expressions

   -------------------
   -- Eval_Property --
   -------------------

   function Get_Property (Node : Ada_Node; Name : Text_Type) return W_Object
   is
      Property_Node : Any_Member_Reference;
   begin
      --  Check if we're on the name of a property (by convention, a name of
      --  the form p_<property_name>), and return it if that's the case.

      if Name'Length > 2
        and then Name (Name'First .. Name'First + 1) = "p_"
      then
         declare
            P_Name : constant Text_Type := Name (Name'First .. Name'Last);
         begin
            Property_Node :=
              Lookup_Member (Id_For_Kind (Node.Kind), P_Name);

            if Property_Node /= None then
               return new W_Property_Type'(Property_Node => Property_Node);
            end if;
         end;
      end if;

      return null;
   end Get_Property;

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Property_Type;
      Params    : T_Arg_Vectors.Vector)
   is

      function Eval_Param (Actual : Actual_To_Formal_Assoc)
         return Libadalang.Introspection.Value_Type;
      --  Evaluate the parameter at the index in parameter with the actual
      --  provided. Will use default value if actual is null, raising an
      --  exception if nonde.

      ----------------
      -- Eval_Param --
      ----------------

      Slice : Buffer_Slice;

      function Eval_Param
        (Actual : Actual_To_Formal_Assoc)
         return Libadalang.Introspection.Value_Type
      is
         Object : W_Object;
      begin
         if Actual.Actual_Expr /= null then
            Object := Evaluate_Expression (Actual.Actual_Expr);

            case Actual.Formal_Type.Kind is
               when Boolean_Value =>
                  --  Put false if the object is a match false, true in all
                  --  other cases.

                  if Object = Match_False then
                     return Create_Boolean (False);
                  else
                     return Create_Boolean (True);
                  end if;

               when Integer_Value =>
                  --  If we have an integer value, extract it, otherwise raise
                  --  an error.

                  if Object.all in W_Integer_Type'Class then
                     return Create_Integer (W_Integer (Object).Value);
                  else
                     Error ("integer value expected");
                  end if;

               when Node_Value =>
                  --  If we have an ada node, extract it, otherwise raise an
                  --  error.

                  if Object.all in W_Kit_Node_Type'Class then
                     return Create_Node (W_Kit_Node (Object).Node);
                  else
                     Error ("node value expected");
                  end if;

               when Text_Type_Value =>
                  --  We can always convert an object to a text value.

                  Push_Buffer_Cursor;
                  Slice := Object.Write_String;
                  Pop_Buffer_Cursor;

                  return Create_Text_Type
                    (Buffer.Str (Slice.First.Offset .. Slice.Last.Offset));

               when Ada_Node_Array_Value |
                    Base_Formal_Param_Decl_Array_Value |
                    Base_Type_Decl_Array_Value |
                    Basic_Decl_Array_Value |
                    Defining_Name_Array_Value |
                    Expr_Array_Value |
                    Generic_Instantiation_Array_Value |
                    Param_Spec_Array_Value |
                    Type_Decl_Array_Value =>
                  --  No support for arrays as parameters yet - could be
                  --  implemented if needed.

                  Error ("array not yet supported as parameters");

               when others =>
                  --  If we don't have specific treatment for the parameter,
                  --  check we we're just passing data around without doing any
                  --  specific work in UWrap.

                  if Object.all in W_Unhandled_Value_Type
                    and then Kind (W_Unhandled_Value (Object).Data)
                      = Actual.Formal_Type.Kind
                  then
                     return W_Unhandled_Value (Object).Data;
                  else
                     Error
                       ("value incompatible with kind "
                        & Value_Kind'Wide_Wide_Image
                          (Actual.Formal_Type.Kind)
                        & " not yet supported in parameters");
                  end if;
            end case;
         else
            declare
               Default : constant Libadalang.Introspection.Value_Type :=
                 Property_Argument_Default_Value
                   (An_Entity.Property_Node, Actual.Formal_Index);
            begin
               if Default = No_Value then
                  Error
                    ("value missing for parameter "
                     & Property_Argument_Name
                            (An_Entity.Property_Node, Actual.Formal_Index));
               else
                  return Default;
               end if;
            end;
         end if;

         return No_Value;
      end Eval_Param;

      Actuals : constant Actual_To_Formal_Assoc_Array :=
        Match_Params (An_Entity.Property_Node, Params);
      Result  : Any_Value_Type;
      Values  : Value_Array (1 .. Actuals'Last);
      Node    : W_Kit_Node;

      Vector : W_Vector;
   begin
      if Top_Object.Dereference.all not in W_Kit_Node_Type'Class then
         Error
           ("expected node (ada), found " & Top_Object.Dereference.Type_Name);
      end if;

      Node := W_Kit_Node (Top_Object.Dereference);

      --  Go through the parameters given and value the Values array

      for E of Actuals loop
         Values (E.Formal_Index) := Eval_Param (E);
      end loop;

      --  Run the property

      Result := Eval_Property
        (Node.Node, An_Entity.Property_Node, Values);

      --  Convert the result into the corresponding W_ object

      case Kind (Result) is
         when Boolean_Value =>
            --  We retrieved a boolean. Push an abitrary true value if true,
            --  match false otherwise.
            --  TODO: We should probably have a proper true boolean
            --  here instead.

            if As_Boolean (Result) then
               Push_Object (W_Object'(new W_Integer_Type'(Value => 1)));
            else
               Push_Match_False;
            end if;

         when Integer_Value =>
            --  We retrieved an integer. Convert that into a W_Integer.

            Push_Object
              (W_Object'(new W_Integer_Type'(Value => As_Integer (Result))));

         when Character_Value =>
            --  We retrieved a character. Convert that into a string of one
            --  character.

            Push_Object (To_W_String ((1 => As_Character (Result))));

         when Node_Value =>
            --  We retrieved a node, either a null one which we model as
            --  a false match (

            if As_Node (Result).Is_Null then
               Push_Match_False;
            else
               Push_Object (Get_Entity_For_Node (As_Node (Result)));
            end if;

         when Text_Type_Value =>
            --  We retrieved a text, convert into a W_String object

            Push_Object (To_W_String (As_Text_Type (Result)));

         when Ada_Node_Array_Value |
              Base_Formal_Param_Decl_Array_Value |
              Base_Type_Decl_Array_Value |
              Basic_Decl_Array_Value |
              Defining_Name_Array_Value |
              Expr_Array_Value |
              Generic_Instantiation_Array_Value |
              Param_Spec_Array_Value |
              Type_Decl_Array_Value =>

            --  We're on an array of nodes. At this stage, each returned
            --  type needs to be handled through its own query, hence the
            --  to level case statement with individual iteration for each
            --  kind of node. Ultimately, it would be nice if langkit provided
            --  some way to have a unique node array for all.

            Vector := new W_Vector_Type;

            case Kind (Result) is
               when Ada_Node_Array_Value =>
                  for N of As_Ada_Node_Array (Result) loop
                     Vector.A_Vector.Append
                       (W_Object (Get_Entity_For_Node (N)));
                  end loop;

               when Base_Formal_Param_Decl_Array_Value =>
                  for N of As_Base_Formal_Param_Decl_Array (Result) loop
                     Vector.A_Vector.Append
                       (W_Object (Get_Entity_For_Node (N)));
                  end loop;

               when Base_Type_Decl_Array_Value =>
                  for N of As_Base_Type_Decl_Array (Result) loop
                     Vector.A_Vector.Append
                       (W_Object (Get_Entity_For_Node (N)));
                  end loop;

               when Basic_Decl_Array_Value =>
                  for N of As_Basic_Decl_Array (Result) loop
                     Vector.A_Vector.Append
                       (W_Object (Get_Entity_For_Node (N)));
                  end loop;

               when Defining_Name_Array_Value =>
                  for N of As_Defining_Name_Array (Result) loop
                     Vector.A_Vector.Append
                       (W_Object (Get_Entity_For_Node (N)));
                  end loop;

               when Expr_Array_Value =>
                  for N of As_Expr_Array (Result) loop
                     Vector.A_Vector.Append
                       (W_Object (Get_Entity_For_Node (N)));
                  end loop;

               when Generic_Instantiation_Array_Value =>
                  for N of As_Generic_Instantiation_Array (Result) loop
                     Vector.A_Vector.Append
                       (W_Object (Get_Entity_For_Node (N)));
                  end loop;

               when Param_Spec_Array_Value =>
                  for N of As_Param_Spec_Array (Result) loop
                     Vector.A_Vector.Append
                       (W_Object (Get_Entity_For_Node (N)));
                  end loop;

               when Type_Decl_Array_Value =>
                  for N of As_Type_Decl_Array (Result) loop
                     Vector.A_Vector.Append
                       (W_Object (Get_Entity_For_Node (N)));
                  end loop;

               when others =>
                  Error
                    ("internal error, missing case for "
                     & Value_Kind'Wide_Wide_Image (Kind (Result)));

            end case;

            Push_Object (Vector);

         when Unbounded_Text_Type_Array_Value =>
            --  We retrieved a list of text. Create a vector for them.

            Vector := new W_Vector_Type;

            for T of As_Unbounded_Text_Type_Array (Result) loop
               Vector.A_Vector.Append (W_Object (To_W_String (To_Text (T))));
            end loop;

            Push_Object (Vector);

         when others =>
            --  When we don't know what to do with the value, create an
            --  unmanaged value that can still be passed as parameter of
            --  another property.

            Push_Object
              (W_Object'(new W_Unhandled_Value_Type'(Data => Result)));
      end case;
   end Push_Call_Result;

   ------------------
   -- Match_Params --
   ------------------

   function Match_Params
     (Profile : Any_Member_Reference; Args : T_Arg_Vectors.Vector)
      return Actual_To_Formal_Assoc_Array
   is
      Params_Types : constant Type_Constraint_Array :=
        Property_Argument_Types (Profile);
      Result : Actual_To_Formal_Assoc_Array (1 .. Params_Types'Length);

      In_Named_Section : Boolean := False;
      Index : Integer := 1;
   begin
      --  Initializes the indices of the associations.

      for I in Result'Range loop
         Result (I).Formal_Index := I;
      end loop;

      --  Resolves the actual parameters that have been provided

      for Arg of Args loop
         Push_Error_Location (Arg.Node);

         declare
            Name_Str : constant Text_Type := Arg.Name_Node.Text;
         begin
            --  There is a name for that parameter, retrieves the index.

            if Name_Str /= "" then
               In_Named_Section := True;
               Index := -1;

               for I in Result'Range loop
                  if Property_Argument_Name (Profile, I) = Name_Str then
                     Index := I;

                     exit;
                  end if;
               end loop;

               --  No index was found. Raise an error.

               if Index = -1 then
                  Error ("parameter " & Arg.Name_Node.Text & " not found");
               end if;

               Result (Index).Actual_Expr := Arg.Expr;
               Result (Index).Formal_Type := Params_Types (Index);
            else
               --  If not a named parameter, check that we're indeed on a
               --  name section and that we're not too far in the list before
               --  assigning.

               if In_Named_Section then
                  Error ("can't have positional parameters after named ones");
               elsif Index > Result'Last then
                  Error ("too many parameters");
               end if;

               Result (Index).Actual_Expr := Arg.Expr;
               Result (Index).Formal_Type := Params_Types (Index);
               Index := Index + 1;
            end if;
         end;
      end loop;

      return Result;
   end Match_Params;

end Wrapping.Input.Ada;
