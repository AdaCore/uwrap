with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers; use Ada.Containers;

with Langkit_Support; use Langkit_Support;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Libtemplatelang.Common; use Libtemplatelang.Common;

with Wrapping.Utils; use Wrapping.Utils;
with Langkit_Support.Text; use Langkit_Support.Text;

package body Wrapping.Semantic.Analysis is

   --  TODO: Since names are resolved in the runtime analysis, we probably
   --  don't need lexical scopes here...
   Lexical_Scope_Stack : T_Entity_Vectors.Vector;

   Entity_Stack : T_Entity_Vectors.Vector;

   function Build_Template_Structure (Node : Template_Node) return Structure.T_Template;
   function Build_Module_Structure (Node : Template_Node; Module_Name : Text_Type) return Structure.T_Module;
   function Build_Command_Structure (Node : Template_Node'Class) return Structure.T_Command;
   function Build_Visitor_Structure (Node : Template_Node) return Structure.T_Visitor;
   function Build_Variable_Structure (Node : Libtemplatelang.Analysis.Var) return Structure.T_Var;
   function Build_Expr (Node : Template_Node'Class) return T_Expr;
   function Build_Arg (Node : Template_Node'Class) return T_Arg;
   function Build_Create_Tree (Node : Template_Node'Class) return T_Create_Tree;
   function Build_Command_Sequence (Node : Command_Sequence'Class) return T_Command_Sequence;
   function Build_Template_Call (Node : Template_Call'Class) return T_Template_Call;

   procedure Analyze_String
     (Node   : Template_Node'Class;
      Result : T_Expr);

   procedure Load_Module (Unit : Analysis_Unit; Name : String);

   Context : constant Analysis_Context := Create_Context;

   procedure Load_Module (Path : String; Name : String) is
      Unit : Analysis_Unit;
   begin
      Unit := Get_From_File (Context, Path);

      if Has_Diagnostics (Unit) then
         for D of Libtemplatelang.Analysis.Diagnostics (Unit) loop
            Put_Line (Path & ":" & To_Pretty_String (D));
         end loop;
      end if;

      Load_Module (Unit, Name);
   end Load_Module;

   procedure Analyze is
   begin
      Root.Resolve_References;
   end Analyze;

   procedure Load_Module (Unit : Analysis_Unit; Name : String) is
      File_Module : Structure.T_Module;
   begin
      Entity_Stack.Append (T_Entity (Root));
      File_Module := Build_Module_Structure (Unit.Root, To_Text (Name));
      Entity_Stack.Delete_Last;
   end Load_Module;

   procedure Push_Error_Location (Node : Template_Node'Class) is
   begin
      if Node.Is_Null then
         Push_Error_Location ("no source", (0, 0));
      else
         Push_Error_Location
           (Node.Unit.Get_Filename,
            (Node.Sloc_Range.Start_Line, Node.Sloc_Range.Start_Column));
      end if;
   end Push_Error_Location;

   procedure Push_Entity (An_Entity : access T_Entity_Type'Class; Node : Template_Node'Class) is
   begin
      Push_Error_Location (Node);
      Add_Child (Entity_Stack.Last_Element, T_Entity (An_Entity));

      An_Entity.Node := Template_Node (Node);
      Entity_Stack.Append (T_Entity (An_Entity));
   end Push_Entity;

   procedure Push_Named_Entity
     (An_Entity : access T_Entity_Type'Class;
      Node      : Template_Node'Class;
      Name      : Text_Type) is
   begin
      Push_Error_Location (Node);
      Add_Child (Entity_Stack.Last_Element, T_Entity (An_Entity), Name);

      An_Entity.Node := Template_Node (Node);
      Entity_Stack.Append (T_Entity (An_Entity));
   end Push_Named_Entity;

   procedure Push_Named_Entity (An_Entity : access T_Named_Entity_Type'Class; Node : Template_Node'Class; Name_Node : Template_Node'Class) is
   begin
      Push_Error_Location (Node);
      Add_Child (Entity_Stack.Last_Element, T_Entity (An_Entity), Name_Node);
      An_Entity.Node := Template_Node (Node);
      Entity_Stack.Append (T_Entity (An_Entity));
      An_Entity.Name_Node := Template_Node (Name_Node);
   end Push_Named_Entity;

   procedure Pop_Entity is
   begin
      Entity_Stack.Delete_Last;
      Pop_Error_Location;
   end Pop_Entity;

   procedure Pop_Lexical_Scope_Entity is
   begin
      Pop_Entity;
      Lexical_Scope_Stack.Delete_Last;
   end Pop_Lexical_Scope_Entity;

   function Build_Module_Structure
     (Node        : Template_Node;
      Module_Name : Text_Type)
      return T_Module
   is
      A_Module : T_Module := new T_Module_Type;
      A_Namespace : T_Namespace;

      Dummy_Command : T_Command;

      Program_Node : Template_Node;
   begin
      A_Namespace := Get_Namespace_Prefix (Module_Name, True);

      --  The module needs to be stacked manually, as it's not a child of
      --  the currently stacked entity (the root node) but of the namespace.
      Add_Child (A_Namespace, A_Module, Suffix (Module_Name));
      A_Module.Name := To_Unbounded_Text (Suffix (Module_Name));
      A_Module.Node := Node;
      Entity_Stack.Append (T_Entity (A_Module));

      Program_Node := Template_Node (Node.As_Module.F_Program);

      for C of Program_Node.Children loop
         case C.Kind is
            when Template_Template =>
               A_Module.Templates_Ordered.Append
                 (Build_Template_Structure (C));
               A_Module.Templates_Indexed.Insert
                 (C.As_Template.F_Name.Text,
                  A_Module.Templates_Ordered.Last_Element);

            when Template_Command =>
               Dummy_Command := Build_Command_Structure (C);

            when Template_Visitor =>
               A_Module.Visitors_Indexed.Insert
                 (C.As_Visitor.F_Name.Text,
                  Build_Visitor_Structure (C));

            when Template_Var =>
               A_Module.Variables_Ordered.Append
                 (Build_Variable_Structure (C.As_Var));
               A_Module.Variables_Indexed.Insert
                 (C.As_Var.F_Name.Text,
                  A_Module.Variables_Ordered.Last_Element);

            when Template_Import | Template_Import_List =>
               null;
               -- To be analyzed when resolving names

            when others =>
               Error ("unsupported node for modules: '" & C.Kind'Wide_Wide_Image & "'");
         end case;
      end loop;

      Pop_Entity;

      return A_Module;
   end Build_Module_Structure;

   function Build_Template_Structure (Node : Template_Node) return Structure.T_Template is
      A_Template : T_Template := new T_Template_Type;
   begin
      Push_Named_Entity (A_Template, Node, Node.As_Template.F_Name);

      for C of Node.As_Template.F_Definition loop
         case C.Kind is
            when Template_Var =>
               A_Template.Variables_Ordered.Append
                 (Build_Variable_Structure (Template_Node (C).As_Var));
               A_Template.Variables_Indexed.Insert
                 (C.As_Var.F_Name.Text,
                  A_Template.Variables_Ordered.Last_Element);

            when others =>
               Error ("unsupported node for templates: '" & C.Kind'Wide_Wide_Image & "'");
         end case;
      end loop;

      Pop_Entity;

      return A_Template;
   end Build_Template_Structure;

   function Build_Command_Sequence (Node : Command_Sequence'Class) return T_Command_Sequence is
      Sequence : T_Command_Sequence := new T_Command_Sequence_Type;
   begin
      Push_Entity (Sequence, Node);

      for C of Node.F_Commands loop
         Sequence.Commands.Append (Build_Command_Structure (C));
      end loop;

      if not Node.F_Then.Is_Null then
         Sequence.Next_Sequence := Build_Command_Sequence (Node.F_Then);
      end if;

      Pop_Entity;

      return Sequence;
   end Build_Command_Sequence;

   function Build_Template_Call (Node : Template_Call'Class) return T_Template_Call is
      A_Template_Call : T_Template_Call := new T_Template_Call_Type;
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

   function Build_Command_Structure (Node : Template_Node'Class) return T_Command is
      A_Command : T_Command := new T_Command_Type;

      function Visit (Node : Template_Node'Class) return Visit_Status is
      begin
         case Node.Kind is
            when Template_Match_Section =>
               A_Command.Match_Expression := Build_Expr
                 (Node.As_Match_Section.F_Expression);

               Node.As_Match_Section.F_Actions.Traverse (Visit'Access);

               if not Node.As_Match_Section.F_Alternate_Actions.Is_Null then
                  A_Command.Else_Actions :=
                    Build_Command_Structure (Node.As_Match_Section.F_Alternate_Actions.F_Actions);
               end if;

               return Over;
            when Template_Pick_Section =>
               A_Command.Pick_Expression :=
                 Build_Expr (Node.As_Pick_Section.F_Expression);

               Node.As_Pick_Section.F_Actions.Traverse (Visit'Access);

               return Over;

            when Template_Wrap_Section | Template_Weave_Section =>
               if Node.Kind = Template_Wrap_Section then
                  A_Command.Template_Section := new Wrap_Type;
               else
                  A_Command.Template_Section := new Weave_Type;
               end if;

               Push_Entity (T_Entity (A_Command.Template_Section), Node);

               Node.As_Template_Section.F_Actions.Traverse
                 (Visit'Access);

               Pop_Entity;

               return Over;

            when Template_Template_Call =>
               A_Command.Template_Section.Call := Build_Template_Call
                 (Node.As_Template_Call);

               return Over;

            when Template_Command_Sequence =>
               A_Command.Command_Sequence := Build_Command_Sequence
                 (Node.As_Command_Sequence);

               return Over;
            when others =>
               return Into;
         end case;
      end Visit;
   begin
      Push_Entity (A_Command, Node);

      for C of Node.Children loop
         C.Traverse (Visit'Access);
      end loop;

      Pop_Entity;

      return A_Command;
   end Build_Command_Structure;

   function Build_Visitor_Structure (Node : Template_Node) return T_Visitor is
      A_Visitor     : T_Visitor := new T_Visitor_Type;
   begin
      Push_Named_Entity (A_Visitor, Node, Node.As_Visitor.F_Name);

      for A of Node.As_Visitor.F_Args loop
         declare
            A_Var : T_Var := new T_Var_Type;
         begin
            Push_Named_Entity (A_Var, A, A);

            A_Var.Kind := Text_Kind;
            A_Visitor.Arguments_Ordered.Append (A_Var);
            A_Visitor.Arguments_Indexed.Insert (A.Text, A_Var);

            Pop_Entity;
         end;
      end loop;

      A_Visitor.Program := Build_Command_Sequence (Node.As_Visitor.F_Program);

      Pop_Entity;

      return A_Visitor;
   end Build_Visitor_Structure;

   function Build_Variable_Structure (Node : Libtemplatelang.Analysis.Var) return Structure.T_Var is
      A_Var : Structure.T_Var := new T_Var_Type;
      Typ : Text_Type := Node.F_Typ.Text;
   begin
      Push_Named_Entity (A_Var, Node, Node.As_Var.F_Name);

      if Typ = "integer" then
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

         --  TODO: implement some kind of type checking and conversions
         --  if needed depending on the type of the set.
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
         Error ("unknown var type: '"
                & Node.F_Typ.Text & "', use text or pattern instead");
      end if;

      for A of Node.As_Var.F_Args loop
         A_Var.Args.Append (Build_Arg (A));
      end loop;

      if not Node.F_Init.Is_Null then
         A_Var.Init_Expr := Build_Expr (Node.F_Init);
      end if;

      Pop_Entity;

      return A_Var;
   end Build_Variable_Structure;

   function Build_Expr (Node : Template_Node'Class) return T_Expr is
      Expr : T_Expr;
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
            Expr.Match_Expr := Build_Expr (Node.As_Match_Capture.F_Expression);

         when Template_Selector =>
            Expr.Selector_Left := Build_Expr (Node.As_Selector.F_Left);
            Expr.Selector_Right := Build_Expr (Node.As_Selector.F_Right);

            --  The tree that libtemplatelang creates needs to be
            --  inverted for selector, so that the analysis goes from the
            --  right to the left. If we are a the root of a selector, then
            --  do the inverstion

            if Parent = null or else Parent.Kind /= Template_Selector then
               declare
                  Current : T_Expr := Expr;
                  Right : T_Expr;
               begin
                  while Current.Selector_Right.Kind = Template_Selector loop
                     Right := Current.Selector_Right;
                     Current.Selector_Right := Right.Selector_Left;
                     Right.Selector_Left := Current;

                     Current := Right;
                  end loop;

                  Expr := Current;
               end;
            end if;

         when Template_Binary_Expr =>
            Expr.Binary_Left := Build_Expr (Node.As_Binary_Expr.F_Lhs);
            Expr.Binary_Right := Build_Expr (Node.As_Binary_Expr.F_Rhs);

         when Template_Unary_Expr =>
            Expr.Unary_Right := Build_Expr (Node.As_Unary_Expr.F_Rhs);

         when Template_Literal =>
            null;

         when Template_Token_Identifier | Template_Identifier =>
            null;

         when Template_Number =>
            Expr.Number := Integer'Wide_Wide_Value (Node.Text);

         when Template_Str =>
            Analyze_String (Node, Expr);

         when Template_Call_Expr =>
            Expr.Called := Build_Expr (Node.As_Call_Expr.F_Called);

            for A of Node.As_Call_Expr.F_Args loop
               Expr.Args.Append (Build_Arg (A));
            end loop;

         when Template_Lambda_Expr =>
            Expr.Lambda_Expr := Build_Expr (Node.As_Lambda_Expr.F_Expression);

         when Template_New_Expr =>
            Expr.Has_New := True;
            Expr.Tree := Build_Create_Tree (Node.As_New_Expr.F_Tree);

         when Template_At_Ref =>
            null;

         when Template_Qualified_Match =>
            Expr.Qualified_Match_Expr := Build_Expr (Node.As_Qualified_Match.F_Rhs);

         when Template_Fold_Expr =>
            Expr.Default := Build_Expr (Node.As_Fold_Expr.F_Default);
            Expr.Combine := Build_Expr (Node.As_Fold_Expr.F_Combine);
            Expr.Separator := Build_Expr (Node.As_Fold_Expr.F_Separator);

         when Template_All_Expr =>
            Expr.All_Match := Build_Expr (Node.As_All_Expr.F_Expression);

         when Template_Reg_Expr =>
            Expr.Reg_Expr_Left := Build_Expr (Node.As_Reg_Expr.F_Left);
            Expr.Reg_Expr_Right := Build_Expr (Node.As_Reg_Expr.F_Right);

         when Template_Reg_Expr_Anchor =>
            null;

         when Template_Reg_Expr_Quantifier =>
            Expr.Quantifier_Expr := Build_Expr
              (Node.As_Reg_Expr_Quantifier.F_Expr);

            if not Node.As_Reg_Expr_Quantifier.F_Min.Is_Null then
               Expr.Min := Integer'Wide_Wide_Value (Node.As_Reg_Expr_Quantifier.F_Min.Text);
            else
               Expr.Min := 0;
            end if;

            if not Node.As_Reg_Expr_Quantifier.F_Max.Is_Null then
               Expr.Max := Integer'Wide_Wide_Value (Node.As_Reg_Expr_Quantifier.F_Max.Text);
            else
               Expr.Max := 0;
            end if;
         when others =>
            Error ("Unsupported expression node");

      end case;

      if Expr.Has_New and then Parent /= null then
         Parent.Has_New := True;
      end if;

      Pop_Entity;

      return Expr;
   end Build_Expr;

   function Build_Arg (Node : Template_Node'Class) return T_Arg is
      Arg : T_Arg := new T_Arg_Type;
   begin
      Push_Entity (Arg, Node);

      Arg.Name_Node := Node.As_Argument.F_Name;

      if not Node.As_Argument.F_Name.Is_Null then
         Arg.Name := To_Unbounded_Text (Node.As_Argument.F_Name.Text);
      end if;

      Arg.Expr := Build_Expr (Node.As_Argument.F_Value);

      Pop_Entity;

      return Arg;
   end Build_Arg;

   Expression_Unit_Number : Integer := 1;

   procedure Analyze_String
     (Node   : Template_Node'Class;
      Result : T_Expr)
   is
      Str : constant Text_Type := Node.Text;
      Context : constant Analysis_Context := Node.Unit.Context;

      Next_Index : Integer;
      Current : Integer;

      procedure On_Error
        (Message : Text_Type;
         Filename : String;
         Loc : Source_Location)
      is
      begin
         Push_Error_Location
           (Node.Unit.Get_Filename,
            (Node.Sloc_Range.Start_Line, Node.Sloc_Range.Start_Column));

         Ada.Wide_Wide_Text_IO.Put_Line
           (To_Text (Get_Sloc_Str)
            & ": " & Message);

         raise Wrapping_Error;
      end On_Error;

      Prev_Error : Error_Callback_Type;

      Str_First, Str_Last : Integer;
      Left_Spaces : Integer := 0;
      Found_Characters_On_Line : Boolean := False;
      Line_Number : Integer := 1;
   begin
      Str_First := Str'First;
      Str_Last := Str'Last;

      if Str'Length = 0 then
         return;
      end if;

      case Str (Str_First) is
         when 'i' =>
            Result.Str_Kind := String_Indent;
            Str_First := Str_First + 1;

         when 'r' =>
            Result.Str_Kind := String_Raw;
            Str_First := Str_First + 1;

         when 's' =>
            Result.Str_Kind := String_Simple;
            Str_First := Str_First + 1;

         when 'x' =>
            Result.Str_Kind := String_Regexp;
            Str_First := Str_First + 1;

         when others =>
            Result.Str_Kind := String_Simple;

      end case;

      if Str_First + 3 in Str'Range
        and then Str (Str_First .. Str_First + 2) = """"""""
        and then Str_Last - 3 in Str'Range
      then
         Str_First := Str_First + 3;
         Str_Last := Str_Last - 3;
      else
         Str_First := Str_First + 1;
         Str_Last := Str_Last - 1;
      end if;

      if Result.Str_Kind = String_Raw then
         Result.Str.Append
           ((Str_Kind, 0, 0, To_Unbounded_Text (Str (Str_First .. Str_Last))));

         return;
      end if;

      if Str (Str_First .. Str_Last) = "" then
         return;
      end if;

      Prev_Error := Error_Callback;
      Error_Callback := On_Error'Unrestricted_Access;

      Current := Str_First;
      Next_Index := Str_First;

      while Current <= Str_Last loop
         if Is_Line_Terminator (Str (Current)) then
            --  Create one entry per line of text. This will help analyzing
            --  empty lines later on.

            if Line_Number = 1
              and then not Found_Characters_On_Line
              and then Result.Str_Kind = String_Indent
            then
               --  Do not add the initial empty line when dealing with string
               --  indent
               null;
            else
               Result.Str.Append
                 ((Str_Kind, 0, 0, To_Unbounded_Text (Str (Next_Index .. Current))));
            end if;

            Line_Number := Line_Number + 1;
            Current := Current + 1;
            Next_Index := Current;
            Found_Characters_On_Line := False;
            Left_Spaces := 0;
         elsif Str (Current) = '\' then
            if Current /= Str'First then
               Result.Str.Append
                 ((Str_Kind, 0, 0, To_Unbounded_Text (Str (Next_Index .. Current - 1))));
            end if;

            Current := Current + 1;

            if Str (Current) = 'e' then
               Current := Current + 1;

               if Str (Current) = '<' then
                  Next_Index := Current;

                  while Next_Index < Str_Last and then Str (Next_Index) /= '>' loop
                     Next_Index := Next_Index + 1;
                  end loop;

                  Expression_Unit_Number := Expression_Unit_Number + 1;

                  declare
                     Expression_Unit : Analysis_Unit :=
                       Get_From_Buffer
                         (Context  => Context,
                          Filename => "internal expression" & Expression_Unit_Number'Img,
                          Buffer   => To_String (Str (Current + 1 .. Next_Index - 1)),
                          Rule     => Expression_Rule);
                  begin
                     if Has_Diagnostics (Expression_Unit) then
                        Error (To_Text (Libtemplatelang.Analysis.Diagnostics (Expression_Unit)(1).Message));
                     end if;

                     Result.Str.Append ((Expr_Kind, 0, 0, Left_Spaces, Build_Expr (Expression_Unit.Root)));
                  end;

                  Current := Next_Index + 1;
                  Next_Index := Current;
               else
                  Result.Str.Append ((Str_Kind, 0, 0, To_Unbounded_Text (Str (Current - 1 .. Current))));
                  Next_Index := Current;
                  Current := Current + 1;
               end if;
            elsif Str (Current) = 'n' then
               Result.Str.Append ((Str_Kind, 0, 0, To_Unbounded_Text (To_Text (String'(1 => ASCII.LF)))));
               Current := Current + 1;
               Next_Index := Current;
            elsif Str (Current) in '0' .. '9' then
               Next_Index := Current;

               while Next_Index < Str_Last and then Str (Next_Index) in '0' .. '9' loop
                  Next_Index := Next_Index + 1;
               end loop;

               if Str (Next_Index) not in '0' .. '9' then
                  Next_Index := Next_Index - 1;
               end if;

               declare
                  Group_Value : Natural :=
                    Natural'Wide_Wide_Value (Str (Current .. Next_Index));
               begin
                  Result.Str.Append ((Group_Kind, 0, 0, Group_Value));
               end;

               Current := Next_Index + 1;
               Next_Index := Current;
            elsif Str (Current) = '\' then
               Result.Str.Append ((Str_Kind, 0, 0, To_Unbounded_Text ("\")));
               Next_Index := Current + 1;
               Current := Current + 1;
            else
               Next_Index := Current;
               Current := Current + 1;
            end if;
         else
            if Str (Current) = ' ' and not Found_Characters_On_Line then
               Left_Spaces := Left_Spaces + 1;
            else
               Found_Characters_On_Line := True;
            end if;

            Current := Current + 1;
         end if;
      end loop;

      if Next_Index <= Str_Last then
         -- Add the end of the text to the result

         Result.Str.Append ((Str_Kind, 0, 0, To_Unbounded_Text (Str (Next_Index .. Str_Last))));
      end if;

      Error_Callback := Prev_Error;
   end Analyze_String;

   function Build_Create_Tree (Node : Template_Node'Class) return T_Create_Tree is
      New_Tree : T_Create_Tree := new T_Create_Tree_Type;
      Tree : Create_Template_Tree := Node.As_Create_Template_Tree;
   begin
      Push_Entity (New_Tree, Node);

      for C of Tree.F_Tree.Children loop
         New_Tree.Subtree.Append (Build_Create_Tree (C));
      end loop;

      if not Tree.F_Root.Is_Null then
         New_Tree.Call := Build_Template_Call (Tree.F_Root);
      end if;

      Pop_Entity;

      return New_Tree;
   end Build_Create_Tree;

end Wrapping.Semantic.Analysis;
