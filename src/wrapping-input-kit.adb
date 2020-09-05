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

with Ada.Directories;
with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;
with Ada.Characters.Conversions;        use Ada.Characters.Conversions;
with Ada.Containers.Vectors;            use Ada.Containers;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Text_IO;
with Ada.Containers;                    use Ada.Containers;

with Wrapping.Runtime.Analysis;   use Wrapping.Runtime.Analysis;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;

package body Wrapping.Input.Kit is

   Global_Node_Registry : W_Kit_Node_Entity_Node_Maps.Map;

   function Get_Entity_For_Node (Node : Kit_Node) return W_Kit_Node;

   ------------------
   -- Analyze_File --
   ------------------

   procedure Analyze_File (File : String) is
      Unit    : Analysis_Unit;
      Context : Analysis_Context := Create_Context;

   begin
      Unit := Get_From_File (Context, File);

      if Has_Diagnostics (Unit) then
         for D of Diagnostics (Unit) loop
            Ada.Text_IO.Put_Line (File & ":" & To_Pretty_String (D));
         end loop;
      end if;

      Analyze_Unit (Unit);
      Analyze_Templates;
   end Analyze_File;

   ------------------
   -- Analyze_Unit --
   ------------------

   procedure Analyze_Unit (Unit : Analysis_Unit) is
      Root_Entity : W_Node;
   begin
      Root_Entity := W_Node (Get_Entity_For_Node (Unit.Root));
      Wrapping.Runtime.Analysis.Analyse_Input (Root_Entity);
   end Analyze_Unit;

   -------------------
   -- Create_Tokens --
   -------------------

   procedure Create_Tokens (Node : W_Kit_Node) is
      Token : Token_Reference;
   begin
      if Node.Tokens.Length > 0 or else Node.Trivia_Tokens.Length > 0 then
         return;
      end if;

      Token := First_Token (Node.Node.Unit);

      declare
         W_Prev_Token : W_Kit_Node_Token;
         W_Token      : W_Kit_Node_Token;
      begin
         while Token /= No_Token loop

            W_Token := new W_Kit_Node_Token_Type'(Node => Token, others => <>);

            if W_Prev_Token /= null then
               Add_Next (W_Prev_Token, W_Token);
            end if;

            if Is_Trivia (Token) then
               Node.Trivia_Tokens.Append (W_Token);
            else
               Node.Tokens.Append (W_Token);
            end if;

            W_Prev_Token := W_Token;
            Token        := Next (Token);
         end loop;
      end;
   end Create_Tokens;

   --------
   -- Lt --
   --------

   function Lt (Left, Right : Kit_Node) return Boolean is
   begin
      if Left.Kind < Right.Kind then
         return True;
      elsif Left.Kind > Right.Kind then
         return False;
      else
         declare
            R_Left  : Source_Location_Range := Sloc_Range (Left);
            R_Right : Source_Location_Range := Sloc_Range (Right);
         begin
            if R_Left.Start_Line < R_Right.Start_Line then
               return True;
            elsif R_Left.Start_Line > R_Right.Start_Line then
               return False;
            elsif R_Left.Start_Column < R_Right.Start_Column then
               return True;
            elsif R_Left.Start_Column > R_Right.Start_Column then
               return False;
            elsif R_Left.End_Line < R_Right.End_Line then
               return True;
            elsif R_Left.End_Line > R_Right.End_Line then
               return False;
            elsif R_Left.End_Column < R_Right.End_Column then
               return True;
            elsif R_Left.End_Column > R_Right.End_Column then
               return False;
            else
               declare
                  Left_Name : String :=
                    Ada.Directories.Simple_Name (Get_Filename (Unit (Left)));
                  Right_Name : String :=
                    Ada.Directories.Simple_Name (Get_Filename (Unit (Right)));
               begin
                  return Left_Name < Right_Name;
               end;
            end if;
         end;
      end if;
   end Lt;

   --------
   -- Eq --
   --------

   function Eq (Left, Right : W_Kit_Node) return Boolean is
   begin
      return Left = Right;
   end Eq;

   ---------------------------
   -- Call_Check_Expression --
   ---------------------------

   procedure Call_Check_Expression
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
   begin
      if Params.Length = 0 then
         Push_Match_True (Object);
      elsif Params.Length = 1 then
         Push_Match_It_Result (W_Object (Object), Params.Element (1).Expr);
      elsif Params.Length > 1 then
         Error ("matcher takes only 1 argument");
      end if;
   end Call_Check_Expression;

   ----------------
   -- Call_Token --
   ----------------

   procedure Call_Token
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Prefix     : W_Kit_Node := W_Kit_Node (Object);
      Match_Expr : T_Expr;

      Analyzed_First : Boolean := False;

      Last_Ref : Token_Reference;

      ---------------
      -- Generator --
      ---------------

      procedure Generator (Expr : T_Expr) is
         Result         : W_Object;
         A_Visit_Action : Visit_Action     := Into;
         Cur_Token      : W_Kit_Node_Token :=
           W_Kit_Node_Token (Top_Object.Dereference);
      begin
         if Analyzed_First then
            Cur_Token := W_Kit_Node_Token (Cur_Token.Next);
         else
            Analyzed_First := True;
         end if;

         while Cur_Token /= null loop
            Cur_Token.Pre_Visit;

            A_Visit_Action := Browse_Entity (Cur_Token, Expr, Result);

            exit when A_Visit_Action in Stop | Over
              or else Cur_Token.Node = Last_Ref;

            Cur_Token := W_Kit_Node_Token (Cur_Token.Next);
         end loop;

         if Result /= null then
            Push_Object (Result);
         else
            Push_Match_False;
         end if;
      end Generator;

   begin
      if Params.Length = 1 then
         Match_Expr := Params.Element (1).Expr;
      elsif Params.Length > 1 then
         Error ("'token' accepts at most one argument'");
      end if;

      Create_Tokens (Prefix);

      declare
         Ref         : Token_Reference := Token_Start (Prefix.Node);
         First_Token : W_Kit_Node_Token;
      begin
         if Ref = No_Token then
            Push_Match_False;

            return;
         elsif Is_Trivia (Ref) then
            First_Token := Prefix.Trivia_Tokens.Element (Index (Ref) - 1);
         else
            First_Token := Prefix.Tokens.Element (Index (Ref) - 1);
         end if;

         Last_Ref := Token_End (Prefix.Node);

         Evaluate_Generator_Regexp
           (First_Token, Match_Expr, Generator'Unrestricted_Access);
      end;
   end Call_Token;

   ---------------
   -- Pre_Visit --
   ---------------

   procedure Pre_Visit (An_Entity : access W_Kit_Node_Type) is
      New_Entity : W_Node;
   begin
      if not An_Entity.Children_Computed then
         for C of An_Entity.Node.Children loop
            if not C.Is_Null then
               New_Entity :=
                 new W_Kit_Node_Type'
                   (Node          => C, Tokens => An_Entity.Tokens,
                    Trivia_Tokens => An_Entity.Trivia_Tokens, others => <>);
               Add_Child (An_Entity, New_Entity);
               An_Entity.Children_By_Node.Insert (C, W_Kit_Node (New_Entity));
               Global_Node_Registry.Insert (C, W_Kit_Node (New_Entity));
            end if;
         end loop;

         An_Entity.Children_Computed := True;

         --  TODO: We probably want to compute parents there too, just in case
         --  this entity is obtaned from a property cross ref and the parent is
         --  not known yet.
      end if;
   end Pre_Visit;

   ----------------
   -- Eval_Field --
   ----------------

   function Eval_Field (Node : Kit_Node; Name : Text_Type) return W_Object is
      Field_Node : Any_Node_Data_Reference;
   begin
      if Name'Length > 2 and then Name (Name'First .. Name'First + 1) = "f_"
      then
         declare
            F_Name : Text_Type :=
              To_Lower (Name (Name'First + 2 .. Name'Last));
         begin
            Field_Node :=
              Lookup_Node_Data (Id_For_Kind (Node.Kind), To_String (F_Name));

            if Field_Node /= None then
               return
                 new W_Source_Node_Type'
                   (A_Node => Eval_Field (Node, Field_Node));
            end if;
         end;
      end if;

      return null;
   end Eval_Field;

   -------------------------
   -- Get_Entity_For_Node --
   -------------------------

   function Get_Entity_For_Node (Node : Kit_Node) return W_Kit_Node is
      Parent     : W_Kit_Node;
      New_Entity : W_Kit_Node;
   begin
      if Global_Node_Registry.Contains (Node) then
         return Global_Node_Registry.Element (Node);
      elsif not Node.Parent.Is_Null then
         Parent := Get_Entity_For_Node (Node.Parent);
         Parent.Pre_Visit;

         return W_Kit_Node_Type (Parent.all).Children_By_Node.Element (Node);
      else
         New_Entity :=
           new W_Kit_Node_Type'
             (Node          => Node, Tokens => new W_Kit_Node_Vectors.Vector,
              Trivia_Tokens => new W_Kit_Node_Vectors.Vector, others => <>);
         Global_Node_Registry.Insert (Node, New_Entity);
         return New_Entity;
      end if;
   end Get_Entity_For_Node;

   -------------------
   -- Eval_Property --
   -------------------

   function Eval_Property (Node : Kit_Node; Name : Text_Type) return W_Object
   is
      Property_Node : Any_Node_Data_Reference;
   begin
      if Name'Length > 2 and then Name (Name'First .. Name'First + 1) = "p_"
      then
         declare
            P_Name : Text_Type :=
              To_Lower (Name (Name'First + 2 .. Name'Last));
            Value : Value_Type;
         begin
            Property_Node :=
              Lookup_Node_Data (Id_For_Kind (Node.Kind), To_String (P_Name));

            if Property_Node /= None then
               declare
                  Values : Value_Array
                    (1 .. Property_Argument_Types (Property_Node)'Last);
               begin
                  for I in Values'Range loop
                     Values (I) :=
                       Property_Argument_Default_Value (Property_Node, I);
                  end loop;

                  Value := Eval_Property (Node, Property_Node, Values);
               end;

               if Kind (Value) = Text_Type_Value then
                  return W_Object (To_W_String (As_Text_Type (Value)));
               elsif Kind (Value) = Node_Value then
                  if As_Node (Value).Is_Null then
                     return Match_False;
                  else
                     return W_Object (Get_Entity_For_Node (As_Node (Value)));
                  end if;
               elsif Kind (Value) = Boolean_Value then
                  if As_Boolean (Value) then
                     --  TODO: We should probably have a proper true boolean
                     --  here instead.
                     return new W_Integer_Type'(Value => 1);
                  else
                     return Match_False;
                  end if;
               else
                  Error
                    ("unsupported property kind: " &
                     Any_Value_Kind'Wide_Wide_Image (Kind (Value)));
               end if;
            end if;
         end;
      end if;

      return null;
   end Eval_Property;

   ----------------
   -- Push_Value --
   ----------------

   overriding function Push_Value
     (An_Entity : access W_Kit_Node_Type; Name : Text_Type) return Boolean
   is
      Id : Any_Node_Type_Id;
   begin
      if W_Node_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      if Name = "hash" then
         Push_Object
           (To_W_String (Hash_Type'Wide_Wide_Image (Hash (An_Entity.Node))));

         return True;
      elsif Name = "sloc" then
         Push_Object (To_W_String (Full_Sloc_Image (An_Entity.Node)));

         return True;
      elsif Name = "kind" then
         Push_Object
           (To_W_String (To_Wide_Wide_String (An_Entity.Node.Kind_Name)));

         return True;
      elsif Name = "token" then
         Push_Object
           (W_Object'
              (new W_Intrinsic_Function_Type'
                 (Prefix       => W_Object (An_Entity),
                  Call         => Call_Token'Unrestricted_Access,
                  Is_Generator => True)));

         return True;
      end if;

      Id := Lookup_DSL_Name (To_String (Name));

      if Id /= No_Node_Type_Id
        and then Is_Derived_From (Id_For_Kind (An_Entity.Node.Kind), Id)
      then
         --  We are in something of the form An_Entity (Node_Type ()); the type
         --  matched. Stack a function that will verify the sub expression.
         --  TODO: This also accepts An_Entity.Node_Type() which might be
         --  bizzare... Need to decide if this is OK.

         Push_Object
           (W_Object'
              (new W_Intrinsic_Function_Type'
                 (Prefix => W_Object (An_Entity),
                  Call   => Call_Check_Expression'Unrestricted_Access,
                  others => <>)));
         return True;
      end if;

      declare
         Result : W_Object;
      begin
         Result := Eval_Field (An_Entity.Node, Name);

         if Result = null then
            Result := Eval_Property (An_Entity.Node, Name);
         end if;

         if Result /= null then
            --  At this point, we may have not computed children yet. Do it if
            --  it's not the case.
            An_Entity.Pre_Visit;

            if Result.all in W_Source_Node_Type'Class then
               Push_Object
                 (An_Entity.Children_By_Node.Element
                    (W_Source_Node_Type (Result.all).A_Node));
            else
               Push_Object (Result);
            end if;

            return True;
         end if;
      end;

      return False;
   end Push_Value;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String (Object : W_Kit_Node_Type) return Text_Type is
   begin
      return Object.Node.Text;
   end To_String;

   ---------------------
   -- To_Debug_String --
   ---------------------

   overriding function To_Debug_String
     (Object : W_Kit_Node_Type) return Text_Type
   is
   begin
      return
        Object.Node.Kind'Wide_Wide_Image & ": " &
        W_Kit_Node_Type'Class (Object).To_String;
   end To_Debug_String;

   ----------------
   -- Push_Value --
   ----------------

   overriding function Push_Value
     (An_Entity : access W_Kit_Node_Token_Type; Name : Text_Type)
      return Boolean
   is
   begin
      if W_Node_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      if Name = "line" or else Name = "start_line" then
         Push_Object
           (W_Object'
              (new W_Integer_Type'
                 (Value =>
                    Integer (Sloc_Range (Data (An_Entity.Node)).Start_Line))));

         return True;
      elsif Name = "column" or else Name = "start_column" then
         Push_Object
           (W_Object'
              (new W_Integer_Type'
                 (Value =>
                    Integer
                      (Sloc_Range (Data (An_Entity.Node)).Start_Column))));

         return True;
      elsif Name = "end_line" then
         Push_Object
           (W_Object'
              (new W_Integer_Type'
                 (Value =>
                    Integer (Sloc_Range (Data (An_Entity.Node)).End_Line))));

         return True;
      elsif Name = "end_column" then
         Push_Object
           (W_Object'
              (new W_Integer_Type'
                 (Value =>
                    Integer (Sloc_Range (Data (An_Entity.Node)).End_Column))));

         return True;
      elsif Name = "is_trivia" then
         if not Is_Trivia (An_Entity.Node) then
            Push_Match_False;
         else
            Push_Match_True (An_Entity);
         end if;

         return True;
      end if;

      declare
         Full_Kind : Wide_Wide_String :=
           Kind (Data (An_Entity.Node))'Wide_Wide_Image;
         Actual_Kind : Wide_Wide_String :=
           To_Lower (Full_Kind (Full_Kind'First + 4 .. Full_Kind'Last));
      begin
         Actual_Kind (Actual_Kind'First) :=
           To_Upper (Actual_Kind (Actual_Kind'First));

         if Name = "kind" then
            Push_Object (To_W_String (Actual_Kind));

            return True;
         elsif Name = Actual_Kind then
            --  We are in something of the form An_Entity (Node_Type ());
            --  the type matched. Stack a function that will verify the sub
            --  expression. TODO: This also accepts An_Entity.Node_Type()
            --  which might be bizzare... Need to decide if this is OK.

            Push_Object
              (W_Object'
                 (new W_Intrinsic_Function_Type'
                    (Prefix => W_Object (An_Entity),
                     Call   => Call_Check_Expression'Unrestricted_Access,
                     others => <>)));

            return True;
         end if;
      end;

      return False;
   end Push_Value;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Object : W_Kit_Node_Token_Type) return Text_Type
   is
   begin
      return Text (Object.Node);
   end To_String;

   ---------------------
   -- To_Debug_String --
   ---------------------

   overriding function To_Debug_String
     (Object : W_Kit_Node_Token_Type) return Text_Type
   is
   begin
      return Text (Object.Node);
   end To_Debug_String;

end Wrapping.Input.Kit;
