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

with Ada.Directories;
with Ada.Text_IO;
with Ada.Containers;                    use Ada.Containers;
with Ada.Characters.Conversions;        use Ada.Characters.Conversions;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with Wrapping.Runtime.Commands;   use Wrapping.Runtime.Commands;
with Wrapping.Runtime.Matching;   use Wrapping.Runtime.Matching;
with Wrapping.Runtime.Frames;     use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Objects;    use Wrapping.Runtime.Objects;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;

package body Wrapping.Input.Kit is

   Global_Node_Registry : W_Kit_Node_Entity_Node_Maps.Map;

   procedure Create_Tokens (Node : W_Kit_Node);

   procedure Call_Token
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   function Eval_Field (Node : Kit_Node; Name : Text_Type) return W_Object;

   ------------------
   -- Analyze_File --
   ------------------

   procedure Analyze_File (File : String) is
      Unit    : access Analysis_Unit;
      Context : constant Analysis_Context := Create_Context;
   begin
      --  This is a kludge to create a unit without deleting it at the end
      --  of the scope and allowing further processing to be done on its nodes
      --  which are registerd by the UWrap program processing.
      --  There's currently no reason to keep a list of these for further
      --  re-use or reclaim, so it's fine for now, but could be revisited
      --  at a later stage.
      pragma Warnings (Off);
      Unit := new Analysis_Unit'(Get_From_File (Context, File));
      pragma Warnings (On);

      if Has_Diagnostics (Unit.all) then
         for D of Diagnostics (Unit.all) loop
            Ada.Text_IO.Put_Line (File & ":" & To_Pretty_String (D));
         end loop;
      end if;

      Analyze_Unit (Unit.all);
   end Analyze_File;

   ------------------
   -- Analyze_Unit --
   ------------------

   procedure Analyze_Unit (Unit : Analysis_Unit) is
   begin
      Analyse_Input (W_Node (Get_Entity_For_Node (Unit.Root)));
   end Analyze_Unit;

   -------------------
   -- Create_Tokens --
   -------------------

   procedure Create_Tokens (Node : W_Kit_Node) is
      Token        : Token_Reference;
      W_Prev_Token : W_Kit_Node_Token;
      W_Token      : W_Kit_Node_Token;
   begin
      if Node.Tokens.Length > 0 or else Node.Trivia_Tokens.Length > 0 then
         return;
      end if;

      Token := First_Token (Node.Node.Unit);

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
            R_Left  : constant Source_Location_Range := Sloc_Range (Left);
            R_Right : constant Source_Location_Range := Sloc_Range (Right);
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
                  Left_Name : constant String :=
                    Ada.Directories.Simple_Name (Get_Filename (Unit (Left)));
                  Right_Name : constant String :=
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

   ----------------
   -- Call_Token --
   ----------------

   procedure Call_Token
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      procedure Generator (Expr : T_Expr);

      Prefix     : constant W_Kit_Node := W_Kit_Node (Object);
      Match_Expr : T_Expr;

      Analyzed_First : Boolean := False;

      Last_Ref : Token_Reference;

      ---------------
      -- Generator --
      ---------------

      procedure Generator (Expr : T_Expr) is
         A_Visit_Action : Visit_Action     := Into;
         Cur_Token      : W_Kit_Node_Token :=
           W_Kit_Node_Token (Top_Object.Dereference);
      begin
         if Analyzed_First then
            Cur_Token := W_Kit_Node_Token (Cur_Token.Next);
         else
            Analyzed_First := True;
         end if;

         if Cur_Token = null then
            Push_Match_False;
         else
            while Cur_Token /= null loop
               Cur_Token.Pre_Visit;

               A_Visit_Action := Process_Generated_Value (Cur_Token, Expr);

               exit when A_Visit_Action in Stop | Over
                 or else Cur_Token.Node = Last_Ref;

               Pop_Object;

               Cur_Token := W_Kit_Node_Token (Cur_Token.Next);
            end loop;
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
         Ref         : constant Token_Reference := Token_Start (Prefix.Node);
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
      Field_Node : Any_Member_Reference;
   begin
      if Name'Length > 2 and then Name (Name'First .. Name'First + 1) = "f_"
      then
         declare
            F_Name : constant Text_Type :=
              To_Lower (Name (Name'First .. Name'Last));
         begin
            Field_Node :=
              Lookup_Member (Id_For_Kind (Node.Kind), F_Name);

            if Field_Node /= None then
               return
                 new W_Source_Node_Type'
                   (A_Node => Eval_Syntax_Field (Node, Field_Node));
            end if;
         end;
      end if;

      return null;
   end Eval_Field;

   -------------------------
   -- Get_Entity_For_Node --
   -------------------------

   function Get_Entity_For_Node (Node : Kit_Node'Class) return W_Kit_Node is
      Parent     : W_Kit_Node;
      New_Entity : W_Kit_Node;
      K_Node     : Kit_Node renames Kit_Node (Node);
   begin
      if Global_Node_Registry.Contains (K_Node) then
         return Global_Node_Registry.Element (K_Node);
      elsif not Node.Parent.Is_Null then
         Parent := Get_Entity_For_Node (Node.Parent);
         Parent.Pre_Visit;

         return W_Kit_Node_Type (Parent.all).Children_By_Node.Element (K_Node);
      else
         New_Entity :=
           new W_Kit_Node_Type'
             (Node          => K_Node,
              Tokens        => new W_Kit_Node_Vectors.Vector,
              Trivia_Tokens => new W_Kit_Node_Vectors.Vector,
              others        => <>);

         Global_Node_Registry.Insert (K_Node, New_Entity);

         return New_Entity;
      end if;
   end Get_Entity_For_Node;

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
                 (Prefix    => W_Object (An_Entity),
                  Call      => Call_Token'Unrestricted_Access,
                  Generator => True)));

         return True;
      end if;

      Id := Lookup_DSL_Name (Name);

      if Id /= No_Node_Type_Id
        and then Is_Derived_From (Id_For_Kind (An_Entity.Node.Kind), Id)
      then
         --  We are in something of the form An_Entity (Node_Type ()); the type
         --  matched. Stack a function that will verify the sub expression.
         --  TODO: This also accepts An_Entity.Node_Type() which might be
         --  bizzare... Need to decide if this is OK.

         Push_Object (An_Entity);
         return True;
      end if;

      declare
         Result : W_Object;
      begin
         Result := Eval_Field (An_Entity.Node, Name);

         if Result = null then
            Result := Get_Property (An_Entity.Node, Name);
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

   ------------------
   -- Write_String --
   ------------------

   overriding function Write_String
     (Object : W_Kit_Node_Type) return Buffer_Slice
   is
   begin
      return Write_String (Object.Node.Text);
   end Write_String;

   ---------------------
   -- To_Debug_String --
   ---------------------

   overriding function To_Debug_String
     (Object : W_Kit_Node_Type) return Text_Type
   is
      Slice : Buffer_Slice;
   begin
      Push_Buffer_Cursor;
      Slice := W_Kit_Node_Type'Class (Object).Write_String;
      Pop_Buffer_Cursor;

      return Buffer.Str (Slice.First.Offset .. Slice.Last.Offset);
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
         Full_Kind : constant Wide_Wide_String :=
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

            Push_Object (An_Entity);

            return True;
         end if;
      end;

      return False;
   end Push_Value;

   ------------------
   -- Write_String --
   ------------------

   overriding function Write_String
     (Object : W_Kit_Node_Token_Type) return Buffer_Slice
   is
   begin
      return Write_String (Text (Object.Node));
   end Write_String;

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
