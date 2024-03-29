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

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Conversions;        use Ada.Characters.Conversions;

with Langkit_Support;             use Langkit_Support;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libtemplatelang.Common; use Libtemplatelang.Common;

with Wrapping.Utils;  use Wrapping.Utils;

package body Wrapping.Semantic.Analysis is

   Entity_Stack : T_Entity_Vectors.Vector;
   --  Stores the entities as we're entering and leaving their scopes. This is
   --  used in particular for easily find the parent.

   Context : constant Analysis_Context := Create_Context;
   --  Global context used to load all the uwrap files.

   procedure Push_Entity
     (An_Entity : access T_Entity_Type'Class; Node : Template_Node'Class);
   --  Pushes an entity on the stack, link with the parent, and node fields and
   --  pushes the corresponding error location

   procedure Push_Named_Entity
     (An_Entity : access T_Named_Entity_Type'Class;
      Node      : Template_Node'Class;
      Name_Node : Template_Node'Class);
   --  Same as Push_Entity but also sets its name. Link with the parent will
   --  also be done in the indexed child list.

   procedure Pop_Entity;
   --  Pops an entity and the error location.

   function Build_Template (Node : Template'Class) return T_Template;
   --  Builds a template T_ node from a parsed template node

   function Build_Module
     (Node : Module'Class; Module_Name : Text_Type) return T_Module;
   --  Builds a module T_ node trom a parsed template node

   function Build_Command (Node : Command'Class) return T_Command;
   --  Builds a command T_ node trom a parsed template node

   function Build_Function (Node : Function_Node'Class) return T_Function;
   --  Builds a function T_ node trom a parsed template node

   function Build_Variable (Node : Var'Class) return T_Var;
   --  Builds a variable T_ node trom a parsed template node

   function Build_Expr (Node : Template_Node'Class) return T_Expr;
   --  Builds an expression T_ node trom a parsed template node

   function Build_Arg (Node : Argument'Class) return T_Arg;
   --  Builds an argument T_ node trom a parsed template node

   function Build_Create_Tree
     (Node : Create_Template_Tree'Class) return T_Create_Tree;
   --  Builds an tree creation T_ node trom a parsed template node

   function Build_Command_Sequence
     (Node : Command_Sequence'Class) return T_Command_Sequence;
   --  Builds an command sequence T_ node trom a parsed template node

   function Build_Command_Sequence_Element
     (Node : Command_Sequence_Element'Class) return T_Command_Sequence_Element;
   --  Builds a command sequence element T_ node trom a parsed template node

   function Build_Template_Call
     (Node : Template_Call'Class) return T_Template_Call;
   --  Builds template call T_ node trom a parsed template node

   procedure Analyze_String (Node : Template_Node'Class; Result : T_Expr);
   --  Pre-analyze a string. In particular, cuts each line into each own
   --  individual objects, build expressions, and create parts for group
   --  references and pre-computes indentation constraints.

   procedure Load_Module (Unit : Analysis_Unit; Name : String);
   --  Loads a module of a given name from a libtemplatelang analysis unit.

   -----------------
   -- Load_Module --
   -----------------

   procedure Load_Module (Path : String; Name : String) is
      Unit : Analysis_Unit;
   begin
      Unit := Get_From_File (Context, Path);

      if Has_Diagnostics (Unit) then
         for D of Libtemplatelang.Analysis.Diagnostics (Unit) loop
            Put_Line (Path & ": " & To_Pretty_String (D));
         end loop;

         Put_Line ("Errors during syntactic analysis.");

         return;
      end if;

      Load_Module (Unit, Name);
   end Load_Module;

   -------------
   -- Analyze --
   -------------

   procedure Analyze is
   begin
      Root.Resolve_References;
   end Analyze;

   -----------------
   -- Load_Module --
   -----------------

   procedure Load_Module (Unit : Analysis_Unit; Name : String) is
      File_Module : T_Module;
      pragma Unreferenced (File_Module);
   begin
      Entity_Stack.Append (T_Entity (Root));
      File_Module := Build_Module (Unit.Root.As_Module, To_Text (Name));
      Entity_Stack.Delete_Last;
   end Load_Module;

   -------------------------
   -- Push_Error_Location --
   -------------------------

   procedure Push_Error_Location (Node : Template_Node'Class) is
   begin
      if Node.Is_Null then
         Push_Error_Location ("<no source>", (0, 0));
      else
         Push_Error_Location
           (Node.Unit.Get_Filename,
            (Node.Sloc_Range.Start_Line, Node.Sloc_Range.Start_Column));
      end if;
   end Push_Error_Location;

   -----------------
   -- Push_Entity --
   -----------------

   procedure Push_Entity
     (An_Entity : access T_Entity_Type'Class; Node : Template_Node'Class)
   is
   begin
      Push_Error_Location (Node);
      Add_Child (Entity_Stack.Last_Element, T_Entity (An_Entity));
      An_Entity.Node := Template_Node (Node);
      Entity_Stack.Append (T_Entity (An_Entity));
   end Push_Entity;

   -----------------------
   -- Push_Named_Entity --
   -----------------------

   procedure Push_Named_Entity
     (An_Entity : access T_Named_Entity_Type'Class; Node : Template_Node'Class;
      Name_Node : Template_Node'Class)
   is
   begin
      Push_Error_Location (Node);
      Add_Child (Entity_Stack.Last_Element, T_Entity (An_Entity), Name_Node);
      An_Entity.Node := Template_Node (Node);
      Entity_Stack.Append (T_Entity (An_Entity));
      An_Entity.Name_Node := Template_Node (Name_Node);
   end Push_Named_Entity;

   ----------------
   -- Pop_Entity --
   ----------------

   procedure Pop_Entity is
   begin
      Entity_Stack.Delete_Last;
      Pop_Error_Location;
   end Pop_Entity;

   ------------------
   -- Build_Module --
   ------------------

   function Build_Module
     (Node : Module'Class; Module_Name : Text_Type) return T_Module
   is
      A_Module    : constant T_Module := new T_Module_Type;
      A_Namespace : T_Namespace;

      Dummy_Command  : T_Command;
      Dummy_Function : T_Function;
   begin
      A_Namespace := Get_Namespace_Prefix_For_Module (Module_Name, True);

      --  The module needs to be stacked manually, as it's not a child of the
      --  currently stacked entity (the root node) but of the namespace.
      Add_Child (A_Namespace, A_Module, Suffix (Module_Name));
      A_Module.Name := To_Unbounded_Text (Suffix (Module_Name));
      A_Module.Node := Template_Node (Node);
      Entity_Stack.Append (T_Entity (A_Module));

      --  Iterate over the chldren of the module and create templates,
      --  commands, global variables and functions.

      for C of Node.F_Program.Children loop
         case C.Kind is
            when Template_Template =>
               A_Module.Templates_Indexed.Insert
                 (C.As_Template.F_Name.Text, Build_Template (C.As_Template));

            when Template_Command =>
               Dummy_Command := Build_Command (C.As_Command);

            when Template_Var =>
               A_Module.Variables_Ordered.Append (Build_Variable (C.As_Var));

            when Template_Import | Template_Import_List =>
               null;
               --  This will be analysed when resolving names.

            when Template_Function_Node =>
               Dummy_Function := Build_Function (C.As_Function_Node);

            when others =>
               Error
                 ("unsupported node for modules: '" & C.Kind'Wide_Wide_Image &
                  "'");
         end case;
      end loop;

      Pop_Entity;

      return A_Module;
   end Build_Module;

   --------------------
   -- Build_Template --
   --------------------

   function Build_Template (Node : Template'Class) return T_Template
   is
      A_Template : constant T_Template := new T_Template_Type;
   begin
      Push_Named_Entity (A_Template, Node, Node.F_Name);

      if not Node.F_Command.Is_Null then
         A_Template.Program := Build_Command (Node.F_Command);
      end if;

      Pop_Entity;

      return A_Template;
   end Build_Template;

   ----------------------------
   -- Build_Command_Sequence --
   ----------------------------

   function Build_Command_Sequence
     (Node : Command_Sequence'Class) return T_Command_Sequence
   is
      Sequence : constant T_Command_Sequence := new T_Command_Sequence_Type;
   begin
      Push_Entity (Sequence, Node);

      if not Node.F_Sequence.Is_Null then
         Sequence.First_Element :=
           Build_Command_Sequence_Element (Node.F_Sequence);
      end if;

      Pop_Entity;

      return Sequence;
   end Build_Command_Sequence;

   ------------------------------------
   -- Build_Command_Sequence_Element --
   ------------------------------------

   function Build_Command_Sequence_Element
     (Node : Command_Sequence_Element'Class) return T_Command_Sequence_Element
   is
      Sequence_Element : constant T_Command_Sequence_Element :=
        new T_Command_Sequence_Element_Type;
   begin
      Push_Entity (Sequence_Element, Node);

      --  By default, sequence elements are then-elements, not else-elements.
      --  Can be switched to true if created from an elsmatch or else.
      Sequence_Element.Is_Else := False;

      for V of Node.F_Vars loop
         Sequence_Element.Vars.Append (Build_Variable (V));
      end loop;

      for C of Node.F_Commands loop
         Sequence_Element.Commands.Append (Build_Command (C));
      end loop;

      if not Node.F_Next.Is_Null then
         case Node.F_Next.Kind is
            when Template_Then_Sequence =>
               Sequence_Element.Next_Element :=
                 Build_Command_Sequence_Element
                   (Node.F_Next.As_Then_Sequence.F_Actions);

            when Template_Elsmatch_Sequence =>
               Sequence_Element.Next_Element :=
                 Build_Command_Sequence_Element
                   (Node.F_Next.As_Elsmatch_Sequence.F_Actions);
               Sequence_Element.Next_Element.Is_Else          := True;
               Sequence_Element.Next_Element.Match_Expression :=
                 Build_Expr (Node.F_Next.As_Elsmatch_Sequence.F_Expression);

            when Template_Else_Sequence =>
               Sequence_Element.Next_Element :=
                 Build_Command_Sequence_Element
                   (Node.F_Next.As_Else_Sequence.F_Actions);
               Sequence_Element.Next_Element.Is_Else := True;

            when others =>
               Error ("unkown sequence kind");

         end case;
      end if;

      Pop_Entity;

      return Sequence_Element;
   end Build_Command_Sequence_Element;

   -------------------------
   -- Build_Template_Call --
   -------------------------

   function Build_Template_Call
     (Node : Template_Call'Class) return T_Template_Call
   is
      A_Template_Call : constant T_Template_Call := new T_Template_Call_Type;
   begin
      Push_Entity (A_Template_Call, Node);

      if not Node.F_Captured.Is_Null then
         A_Template_Call.Captured_Name :=
           To_Unbounded_Text (Node.F_Captured.Text);
      end if;

      for A of Node.F_Args loop
         A_Template_Call.Args.Append (Build_Arg (A));
      end loop;

      Pop_Entity;

      return A_Template_Call;
   end Build_Template_Call;

   -------------------
   -- Build_Command --
   -------------------

   function Build_Command (Node : Command'Class) return T_Command is

      function Visit (Node : Template_Node'Class) return Visit_Status;
      --  Retreives various sections of the command being parsed.

      A_Command : constant T_Command := new T_Command_Type;

      -----------
      -- Visit --
      -----------

      function Visit (Node : Template_Node'Class) return Visit_Status is
      begin
         case Node.Kind is
            when Template_Defer_Section =>
               A_Command.Defer := True;

               if not Node.As_Defer_Section.F_Expression.Is_Null then
                  A_Command.Defer_Expression :=
                    Build_Expr (Node.As_Defer_Section.F_Expression);
               end if;

               if not Node.As_Defer_Section.F_Actions.Is_Null then
                  Node.As_Defer_Section.F_Actions.Traverse (Visit'Access);
               end if;

            when Template_Match_Section =>
               A_Command.Match_Expression :=
                 Build_Expr (Node.As_Match_Section.F_Expression);

               if not Node.As_Match_Section.F_Actions.Is_Null then
                  Node.As_Match_Section.F_Actions.Traverse (Visit'Access);
               end if;

            when Template_Pick_Section =>
               A_Command.Pick_Expression :=
                 Build_Expr (Node.As_Pick_Section.F_Expression);

               if not Node.As_Pick_Section.F_Actions.Is_Null then
                  Node.As_Pick_Section.F_Actions.Traverse (Visit'Access);
               end if;

            when Template_Wrap_Section | Template_Weave_Section |
              Template_Walk_Section =>

               if Node.Kind = Template_Wrap_Section then
                  A_Command.Template_Section :=
                    new T_Template_Section_Type'
                      (Kind => Wrap_Kind, others => <>);

                  case Node.As_Template_Section.F_Actions.Kind is
                     when Template_Traverse_Into =>
                        A_Command.Template_Section.A_Visit_Action := Into;

                     when Template_Traverse_Over =>
                        A_Command.Template_Section.A_Visit_Action := Over;

                     when others =>
                        null;

                  end case;

               elsif Node.Kind = Template_Weave_Section then
                  A_Command.Template_Section :=
                    new T_Template_Section_Type'
                      (Kind => Weave_Kind, others => <>);
               else
                  A_Command.Template_Section :=
                    new T_Template_Section_Type'
                      (Kind => Walk_Kind, others => <>);
               end if;

               Push_Entity (A_Command.Template_Section, Node);

               Node.As_Template_Section.F_Actions.Traverse (Visit'Access);

               Pop_Entity;

            when Template_Template_Call =>
               A_Command.Template_Section.Call :=
                 Build_Template_Call (Node.As_Template_Call);

            when Template_Command_Sequence =>
               A_Command.Command_Sequence :=
                 Build_Command_Sequence (Node.As_Command_Sequence);

            when others =>
               return Into;
         end case;

         return Over;
      end Visit;
   begin
      A_Command.Defer := False;

      Push_Entity (A_Command, Node);

      for C of Node.Children loop
         if not C.Is_Null then
            C.Traverse (Visit'Access);
         end if;
      end loop;

      Pop_Entity;

      return A_Command;
   end Build_Command;

   --------------------
   -- Build_Function --
   --------------------

   function Build_Function (Node : Function_Node'Class) return T_Function is
      A_Function : constant T_Function := new T_Function_Type;
   begin
      Push_Named_Entity (A_Function, Node, Node.F_Name);

      for A of Node.F_Args loop
         declare
            A_Var : constant T_Var := new T_Var_Type;
         begin
            Push_Named_Entity (A_Var, A, A);

            A_Var.Kind := Text_Kind;
            A_Function.Arguments_Ordered.Append (A_Var);
            A_Function.Arguments_Indexed.Insert (A.Text, A_Var);

            Pop_Entity;
         end;
      end loop;

      A_Function.Program :=
        Build_Command_Sequence (Node.F_Program);

      Pop_Entity;

      return A_Function;
   end Build_Function;

   --------------------
   -- Build_Variable --
   --------------------

   function Build_Variable (Node : Var'Class) return T_Var is
      A_Var : constant Structure.T_Var := new T_Var_Type;
      Typ   : constant Text_Type       := Node.F_Typ.Text;
   begin
      Push_Named_Entity (A_Var, Node, Node.As_Var.F_Name);

      if Typ = "object" then
         A_Var.Kind := Object_Kind;

         if Node.F_Args.Children_Count /= 0 then
            Error ("no argument expected for object var");
         end if;
      elsif Typ = "integer" then
         A_Var.Kind := Integer_Kind;

         if Node.F_Args.Children_Count /= 0 then
            Error ("no argument expected for integer var");
         end if;
      elsif Typ = "text" then
         A_Var.Kind := Text_Kind;

         if Node.F_Args.Children_Count /= 0 then
            Error ("no argument expected for text var");
         end if;
      elsif Typ = "set" then
         A_Var.Kind := Set_Kind;

         if Node.F_Args.Children_Count /= 1 then
            Error ("missing parameter for set");
         end if;

         --  TODO: implement some kind of type checking and conversions if
         --  needed depending on the type of the set.
      elsif Typ = "map" then
         A_Var.Kind := Map_Kind;

         if Node.F_Args.Children_Count /= 2 then
            Error ("need to specify key and element");
         end if;

         if Node.F_Args.Child (1).Text /= "string"
           or else Node.F_Args.Child (2).Text /= "object"
         then
            Error ("only (string, object) is currently supported for maps");
         end if;
      elsif Typ = "vector" then
         A_Var.Kind := Vector_Kind;

         if Node.F_Args.Children_Count /= 1 then
            Error ("missing parameter for set");
         end if;
      else
         Error
           ("unknown var type: '" & Node.F_Typ.Text &
            "', use text or pattern instead");
      end if;

      for A of Node.As_Var.F_Args loop
         A_Var.Args.Append (Build_Arg (A));
      end loop;

      if not Node.F_Init.Is_Null then
         A_Var.Init_Expr := Build_Expr (Node.F_Init);
      end if;

      Pop_Entity;

      return A_Var;
   end Build_Variable;

   ----------------
   -- Build_Expr --
   ----------------

   function Build_Expr (Node : Template_Node'Class) return T_Expr is
      Expr   : T_Expr;
      Parent : T_Expr;
   begin
      if Node.Is_Null then
         return null;
      end if;

      Expr := new T_Expr_Type (Node.Kind);

      if Entity_Stack.Last_Element.all in T_Expr_Type'Class then
         Parent := T_Expr (Entity_Stack.Last_Element);
      end if;

      Push_Entity (Expr, Node);

      case Node.Kind is
         when Template_Match_Capture =>
            Expr.Match_Capture_Expr :=
              Build_Expr (Node.As_Match_Capture.F_Expression);

         when Template_Selector =>
            Expr.Selector_Left  := Build_Expr (Node.As_Selector.F_Left);
            Expr.Selector_Right := Build_Expr (Node.As_Selector.F_Right);

            --  The tree that libtemplatelang creates needs to be inverted for
            --  selector, so that the analysis goes from the right to the left.
            --  If we are a the root of a selector, then do the inverstion

            if Parent = null or else Parent.Kind /= Template_Selector then
               declare
                  Current : T_Expr := Expr;
                  Right   : T_Expr;
               begin
                  while Current.Selector_Right.Kind = Template_Selector loop
                     Right                  := Current.Selector_Right;
                     Current.Selector_Right := Right.Selector_Left;
                     Right.Selector_Left    := Current;

                     Current := Right;
                  end loop;

                  Expr := Current;
               end;
            end if;

         when Template_Binary_Expr =>
            Expr.Binary_Left  := Build_Expr (Node.As_Binary_Expr.F_Lhs);
            Expr.Binary_Right := Build_Expr (Node.As_Binary_Expr.F_Rhs);

         when Template_Unary_Expr =>
            Expr.Unary_Right := Build_Expr (Node.As_Unary_Expr.F_Rhs);

         when Template_Literal =>
            null;

         when Template_Identifier =>
            null;

         when Template_Number =>
            Expr.Number := Integer'Wide_Wide_Value (Node.Text);

         when Template_Str =>
            Analyze_String (Node, Expr);

         when Template_Call_Expr =>
            Expr.Call_Called := Build_Expr (Node.As_Call_Expr.F_Called);

            for A of Node.As_Call_Expr.F_Args loop
               Expr.Call_Args.Append (Build_Arg (A));
            end loop;

         when Template_Defer_Expr =>
            Expr.Deferred_Expr := Build_Expr (Node.As_Defer_Expr.F_Expression);

         when Template_New_Expr =>
            Expr.New_Tree := Build_Create_Tree (Node.As_New_Expr.F_Tree);

         when Template_At_Ref =>
            null;

         when Template_Qualified_Match =>
            Expr.Qualified_Match_Expr :=
              Build_Expr (Node.As_Qualified_Match.F_Rhs);

         when Template_Fold_Expr =>
            Expr.Fold_Default   := Build_Expr (Node.As_Fold_Expr.F_Default);
            Expr.Fold_Combine   := Build_Expr (Node.As_Fold_Expr.F_Combine);
            Expr.Fold_Separator := Build_Expr (Node.As_Fold_Expr.F_Separator);

         when Template_All_Expr =>
            Expr.All_Match := Build_Expr (Node.As_All_Expr.F_Expression);

         when Template_Reg_Expr =>
            Expr.Reg_Expr_Left  := Build_Expr (Node.As_Reg_Expr.F_Left);
            Expr.Reg_Expr_Right := Build_Expr (Node.As_Reg_Expr.F_Right);

         when Template_Reg_Expr_Anchor =>
            null;

         when Template_Reg_Expr_Quantifier =>
            Expr.Quantifier_Expr :=
              Build_Expr (Node.As_Reg_Expr_Quantifier.F_Expr);

            if not Node.As_Reg_Expr_Quantifier.F_Min.Is_Null then
               Expr.Quantifier_Min :=
                 Integer'Wide_Wide_Value
                   (Node.As_Reg_Expr_Quantifier.F_Min.Text);
            else
               Expr.Quantifier_Min := 0;
            end if;

            if not Node.As_Reg_Expr_Quantifier.F_Max.Is_Null then
               Expr.Quantifier_Max :=
                 Integer'Wide_Wide_Value
                   (Node.As_Reg_Expr_Quantifier.F_Max.Text);
            else
               Expr.Quantifier_Max := 0;
            end if;

         when Template_Match_Expr =>
            Expr.Match_Match_Expr :=
              Build_Expr (Node.As_Match_Expr.F_Match_Exp);
            Expr.Match_Pick_Expr := Build_Expr (Node.As_Match_Expr.F_Pick_Exp);

            if not Node.As_Match_Expr.F_Else_Exp.Is_Null then
               Expr.Match_Else_Expr :=
                 Build_Expr (Node.As_Match_Expr.F_Else_Exp);
            end if;

         when Template_Filter_Expr =>
            Expr.Filter_Expr := Build_Expr (Node.As_Filter_Expr.F_Expression);

         when others =>
            Error ("Unsupported expression node");

      end case;

      Pop_Entity;

      return Expr;
   end Build_Expr;

   ---------------
   -- Build_Arg --
   ---------------

   function Build_Arg (Node : Argument'Class) return T_Arg is
      Arg : constant T_Arg := new T_Arg_Type;
   begin
      Push_Entity (Arg, Node);

      Arg.Name_Node := Node.F_Name;
      Arg.Expr := Build_Expr (Node.F_Value);

      Pop_Entity;

      return Arg;
   end Build_Arg;

   Expression_Unit_Number : Integer := 1;
   --  This global variable is used to create expesssions in strings. Each will
   --  have its own unique expression number. This is a bit of a kludgy
   --  behavior, but doing otherwise is difficult with current langkit
   --  capabilities.

   --------------------
   -- Analyze_String --
   --------------------

   procedure Analyze_String (Node : Template_Node'Class; Result : T_Expr) is

      procedure On_Error
        (Message : Text_Type; Filename : String; Loc : Source_Location);
      --  Callback used to override the default error location when entering
      --  a string.
      --  TOTO: This is still a relatively weak way to retreive slocs for
      --  errors in strings, essentially sets the sloc at the begining of
      --  the strings, and disregards the fact that string expressions may
      --  call other functions which would have proper sloc. Lost of room
      --  for improvement.

      --------------
      -- On_Error --
      --------------

      procedure On_Error
        (Message : Text_Type; Filename : String; Loc : Source_Location)
      is
         pragma Unreferenced (Loc, Filename);
      begin
         Push_Error_Location
           (Node.Unit.Get_Filename,
            (Node.Sloc_Range.Start_Line, Node.Sloc_Range.Start_Column));

         Ada.Wide_Wide_Text_IO.Put_Line
           (To_Text (Get_Sloc_Str) & ": " & Message);

         raise Wrapping_Error;
      end On_Error;

      Str     : constant Text_Type        := Node.Text;
      Context : constant Analysis_Context := Node.Unit.Context;

      Start   : Integer;
      --  Start is the index of the first character to add in the newly created
      --  string. It moves as caracters gets added.

      Current : Integer;

      Prev_Error : Error_Callback_Type;

      Str_First, Str_Last   : Integer;
      Left_Spaces           : Integer := 0;

      Indentation_To_Ignore : Integer := Integer'Last;
      --  Most of the times, the whole block of text will be indented. For
      --  example:
      --     a
      --        b
      --     c
      --  In this case, the identation before the leftmost token is considered
      --  to be the indentation used to structure the wrapping program, not
      --  the text istelf. It needs to be removed to end up to:
      --  a
      --     b
      --  c

      Found_Characters_On_Line : Boolean := False;
      Line, Column             : Integer := 1;
      Is_Start_Of_Line         : Boolean := False;
   begin
      Str_First := Str'First;
      Str_Last  := Str'Last;

      if Str'Length = 0 then
         return;
      end if;

      --  First, check the string prefix if any.

      case Str (Str_First) is
         when 'i' =>
            Result.Str_Kind := String_Indent;
            Str_First       := Str_First + 1;

         when 'r' =>
            Result.Str_Kind := String_Raw;
            Str_First       := Str_First + 1;

         when 's' =>
            Result.Str_Kind := String_Simple;
            Str_First       := Str_First + 1;

         when 'x' =>
            Result.Str_Kind := String_Regexp;
            Str_First       := Str_First + 1;

         when others =>
            Result.Str_Kind := String_Simple;

      end case;

      --  Set Str_First and String_Last depending on wether this is a single
      --  line string or a multi line string.

      if Str_First + 3 in Str'Range
        and then Str (Str_First .. Str_First + 2) = """"""""
        and then Str_Last - 3 in Str'Range
      then
         Str_First := Str_First + 3;
         Str_Last  := Str_Last - 3;
      else
         Str_First := Str_First + 1;
         Str_Last  := Str_Last - 1;
      end if;

      --  If we're on a raw string, there's no analysis to do, just store the
      --  string.

      if Result.Str_Kind = String_Raw then
         Result.Str.Append
           ((Str_Kind, Line, Column, Left_Spaces,
            To_Unbounded_Text (Str (Str_First .. Str_Last))));

         return;
      end if;

      --  If this string is empty, there's no more things to do. Return.

      if Str_First > Str_Last then
         return;
      end if;

      Prev_Error     := Error_Callback;
      Error_Callback := On_Error'Unrestricted_Access;

      Current := Str_First;
      Start   := Str_First;

      while Current <= Str_Last loop
         if Is_Line_Terminator (Str (Current)) then
            --  Create one entry per line of text. This will help analyzing
            --  empty lines later on.

            if Line = 1
              and then not Found_Characters_On_Line
              and then Result.Str_Kind = String_Indent
            then
               --  Do not add the initial empty line when dealing with string
               --  indent
               null;
            else
               --  Add all characters found from last insertion up until this
               --  point

               Result.Str.Append
                 ((Str_Kind, Line, Column, Left_Spaces,
                   To_Unbounded_Text (Str (Start .. Current))));
            end if;

            Line                     := Line + 1;
            Column                   := 1;
            Current                  := Current + 1;
            Start                    := Current;
            Found_Characters_On_Line := False;
            Left_Spaces              := 0;
         elsif Str (Current) = '\' then
            --  We're on a special character pattern.

            if Current /= Str'First then
               --  Add all characters found from last insertion up until before
               --  the escape character.

               Result.Str.Append
                 ((Str_Kind, Line, Column, Left_Spaces,
                      To_Unbounded_Text (Str (Start .. Current - 1))));
            end if;

            Current := Current + 1;
            Column  := Column + 1;

            if Str (Current) = 'e' then
               --  We're inserting an expression. Ignore the e, and then
               --  build the expression.

               Current := Current + 1;
               Column  := Column + 1;

               if Str (Current) = '<' then
                  Start := Current;

                  --  TODO: There's an issue here, subexpression can't have
                  --  upper than because of the end symbol
                  while Start < Str_Last and then Str (Start) /= '>'
                  loop
                     Start := Start + 1;
                  end loop;

                  --  Increment the expression unit number.

                  Expression_Unit_Number := Expression_Unit_Number + 1;

                  declare
                     --  Create a new analysis context unique to this
                     --  expression.

                     Expression_Unit : constant Analysis_Unit :=
                       Get_From_Buffer
                         (Context  => Context,
                          Filename =>
                            "internal expression" & Expression_Unit_Number'Img,
                          Buffer =>
                            To_String (Str (Current + 1 .. Start - 1)),
                          Rule => Expression_Rule);
                  begin
                     if Has_Diagnostics (Expression_Unit) then
                        Error
                          (To_Text
                             (Libtemplatelang.Analysis.Diagnostics
                                (Expression_Unit)
                                (1)
                                .Message));
                     end if;

                     --  Build and add the expression to the string.

                     Result.Str.Append
                       ((Expr_Kind, Line, Column, Left_Spaces,
                        Build_Expr (Expression_Unit.Root)));
                  end;

                  Current := Start + 1;
                  Start   := Current;
                  Column  := Column + 1;
               else
                  --  We're not escaping an expression, but just \e. Add the
                  --  'e' to the buffer.

                  Result.Str.Append
                    ((Str_Kind, Line, Column, Left_Spaces,
                      To_Unbounded_Text (Str (Current - 1 .. Current))));

                  Start   := Current;
                  Current := Current + 1;
                  Column  := Column + 1;
               end if;
            elsif Str (Current) = 'n' then
               --  We're on \n. Add a new line to the buffer.

               Result.Str.Append
                 ((Str_Kind, Line, Column, Left_Spaces,
                   To_Unbounded_Text (To_Text (String'(1 => ASCII.LF)))));

               Current := Current + 1;
               Start   := Current;
               Column  := Column + 1;
            elsif Str (Current) in '0' .. '9' then
               --  We're on a number, e.g. \1. This refers to a group captured
               --  before. retreive the number.

               Start := Current;

               while Current < Str_Last
                 and then Str (Current) in '0' .. '9'
               loop
                  Current := Current + 1;
               end loop;

               --  If we're still on a number, that means that this number is
               --  actually at the end of the string and Next_Index went one
               --  character too many.

               if Str (Current) not in '0' .. '9' then
                  Current := Current - 1;
               end if;

               --  Create the group.

               declare
                  Group_Value : constant Natural :=
                    Natural'Wide_Wide_Value (Str (Start .. Current));
               begin
                  Result.Str.Append
                    ((Group_Kind, Line, Column, Left_Spaces, Group_Value));
               end;

               Current := Current + 1;
               Start   := Current;
               Column  := Column + 1;
            elsif Str (Current) = '\' then
               --  We're on \\. Add \.

               Result.Str.Append
                 ((Str_Kind, Line, Column, Left_Spaces,
                  To_Unbounded_Text ("\")));

               Current := Current + 1;
               Start   := Current;
               Column  := Column + 1;
            else
               --  We're not on a special character.

               Current := Current + 1;
               Column  := Column + 1;
            end if;

            if not Found_Characters_On_Line
              and then Left_Spaces < Indentation_To_Ignore
            then
               Indentation_To_Ignore := Left_Spaces;
            end if;

            Found_Characters_On_Line := True;
         else
            if Str (Current) = ' ' and not Found_Characters_On_Line then
               Left_Spaces := Left_Spaces + 1;
            else
               if not Found_Characters_On_Line
                 and then Left_Spaces < Indentation_To_Ignore
               then
                  Indentation_To_Ignore := Left_Spaces;
               end if;

               Found_Characters_On_Line := True;
            end if;

            Current := Current + 1;
            Column := Column + 1;
         end if;
      end loop;

      if Start <= Str_Last then
         --  Add the end of the text to the result

         Result.Str.Append
           ((Str_Kind, Line, Column, Left_Spaces,
             To_Unbounded_Text (Str (Start .. Str_Last))));
      end if;

      if Result.Str_Kind = String_Indent then
         --  If we're on an indentation string, remove the spaces that have
         --  been identified as needing removal;

         Is_Start_Of_Line := True;

         for S of Result.Str loop
            if S.Kind = Str_Kind then
               declare
                  Str : constant Text_Type := To_Text (S.Value);
               begin
                  if Is_Start_Of_Line
                    and then Str'Length >= Indentation_To_Ignore
                  then
                     S.Value := To_Unbounded_Text
                       (Str (Str'First + S.Indent .. Str'Last));
                  end if;

                  Is_Start_Of_Line :=
                    Str'Length > 0
                    and then Is_Line_Terminator (Str (Str'Last));
               end;
            end if;

            if S.Indent >= Indentation_To_Ignore then
               S.Indent := S.Indent - Indentation_To_Ignore;
            else
               S.Indent := 0;
            end if;
         end loop;
      else
         --  If we're not on an indentation string, then reset the indent
         --  values.

         for S of Result.Str loop
            S.Indent := 0;
         end loop;
      end if;

      Error_Callback := Prev_Error;
   end Analyze_String;

   -----------------------
   -- Build_Create_Tree --
   -----------------------

   function Build_Create_Tree
     (Node : Create_Template_Tree'Class) return T_Create_Tree
   is
      New_Tree : constant T_Create_Tree        := new T_Create_Tree_Type;
   begin
      Push_Entity (New_Tree, Node);

      for C of Node.F_Tree loop
         New_Tree.Subtree.Append (Build_Create_Tree (C));
      end loop;

      if not Node.F_Root.Is_Null then
         New_Tree.Call := Build_Template_Call (Node.F_Root);
      end if;

      Pop_Entity;

      return New_Tree;
   end Build_Create_Tree;

end Wrapping.Semantic.Analysis;
