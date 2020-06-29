with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
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
   function Build_Command_Scope_Structure (Node : Template_Node'Class) return T_Entity;
   function Build_Expr (Node : Template_Node'Class) return T_Expr;
   function Build_Arg (Node : Template_Node'Class) return T_Arg;

   procedure Resolve_Template_Names (A_Template : Structure.T_Template);
   procedure Resolve_Module_Names (A_Module : Structure.T_Module);
   procedure Resolve_Command_Names (A_Command : Structure.T_Command);
   procedure Resolve_Visitor_Names (A_Visitor : Structure.T_Visitor);
   procedure Resolve_Namespace_Names (A_Namespace : Structure.T_Namespace);
   function Get_Static_Entity_By_Name (Current_Scope : T_Entity; Name : Selector) return Structure.T_Entity;
   function Get_Template_Or_Visitor_By_Name (Current_Scope : T_Entity; Name : Selector) return Structure.T_Entity;

   procedure Analyze_Replace_String
     (Node   : Template_Node'Class;
      Result : in out Processed_String_Vector.Vector);

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
      Resolve_Namespace_Names (Root);
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
      Push_Error_Location
        (Node.Unit.Get_Filename,
         (Node.Sloc_Range.Start_Line, Node.Sloc_Range.Start_Column));
   end Push_Error_Location;

   procedure Push_Entity (An_Entity : access T_Entity_Type'Class; Node : Template_Node'Class) is
   begin
      if Entity_Stack.Length > 0 then
         Add_Child (Entity_Stack.Last_Element, T_Entity (An_Entity));
      end if;

      An_Entity.Node := Template_Node (Node);
      Entity_Stack.Append (T_Entity (An_Entity));
   end Push_Entity;

   procedure Push_Named_Entity
     (An_Entity : access T_Entity_Type'Class;
      Node      : Template_Node'Class;
      Name      : Text_Type) is
   begin
      if Entity_Stack.Length > 0 then
         Add_Child (Entity_Stack.Last_Element, T_Entity (An_Entity), Name);
      end if;

      An_Entity.Node := Template_Node (Node);
      Entity_Stack.Append (T_Entity (An_Entity));
   end Push_Named_Entity;

   procedure Push_Named_Entity (An_Entity : access T_Named_Entity_Type'Class; Node : Template_Node'Class; Name_Node : Template_Node'Class) is
   begin
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
      return Structure.T_Module
   is
      A_Module : Structure.T_Module := new T_Module_Type;
      A_Namespace : Structure.T_Namespace;

      Dummy_Command : Structure.T_Command;

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
      A_Template : Structure.T_Template := new T_Template_Type;
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

   function Build_Command_Structure (Node : Template_Node'Class) return Structure.T_Command is
      A_Command : Structure.T_Command := new T_Command_Type;

      function Visit (Node : Template_Node'Class) return Visit_Status is
      begin
         case Node.Kind is
            when Template_Match_Section =>
               A_Command.Match_Expression := Build_Expr
                 (Node.As_Match_Section.F_Expression);

               Node.As_Match_Section.F_Actions.Traverse (Visit'Access);

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

               A_Command.Template_Section.Node := Node.As_Template_Section;

               return Over;
            when Template_Nested_Scope =>
               A_Command.Nested_Actions :=
                 Build_Command_Scope_Structure (Node.As_Nested_Scope);

               return Over;
            when Template_Else_Section =>
               case Node.As_Else_Section.F_Actions.Kind is
                  when Template_Command =>
                     A_Command.Else_Actions := T_Entity (Build_Command_Structure (Node.As_Else_Section.F_Actions));

                  when Template_Nested_Scope =>
                     A_Command.Else_Actions := Build_Command_Scope_Structure (Node.As_Else_Section.F_Actions);

                  when others =>
                     Error
                       ("unexpected node kind for command else: '"
                        & Node.As_Else_Section.F_Actions.Kind'Wide_Wide_Image
                        & "'");

               end case;

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

   function Build_Visitor_Structure (Node : Template_Node) return Structure.T_Visitor is
      A_Visitor : Semantic.Structure.T_Visitor := new T_Visitor_Type;
      Program_Node : Template_Node;
      Dummy_Command : Structure.T_Command;
   begin
      Push_Named_Entity (A_Visitor, Node, Node.As_Visitor.F_Name);

      for A of Node.As_Visitor.F_Args loop
         declare
            A_Var : Semantic.Structure.T_Var := new T_Var_Type;
         begin
            Push_Named_Entity (A_Var, A, A);

            A_Var.Kind := Text_Kind;
            A_Visitor.Arguments_Ordered.Append (A_Var);
            A_Visitor.Arguments_Indexed.Insert (A.Text, A_Var);

            Pop_Entity;
         end;
      end loop;

      Program_Node := Template_Node (Node.As_Visitor.F_Program.F_Scope);

      for C of Program_Node.Children loop
         case C.Kind is
            when Template_Command =>
               Dummy_Command := Build_Command_Structure (C);

            when others =>
               Error ("unsupported node for visitor: '" & C.Kind'Wide_Wide_Image & "'");
         end case;
      end loop;

      Pop_Entity;

      return A_Visitor;
   end Build_Visitor_Structure;

   function Build_Variable_Structure (Node : Libtemplatelang.Analysis.Var) return Structure.T_Var is
      A_Var : Structure.T_Var := new T_Var_Type;
   begin
      Push_Named_Entity (A_Var, Node, Node.As_Var.F_Name);

      if Node.F_Typ.Text = "text" then
         A_Var.Kind := Text_Kind;

         if Node.F_Args.Children_Count /= 0 then
            Error ("no argument expected for text var");
         end if;
      elsif Node.F_Typ.Text = "pattern" then
         A_Var.Kind := Pattern_Kind;

         if Node.F_Args.Children_Count /= 1 then
            Error ("missing parameter for pattern");
         end if;
      elsif Node.F_Typ.Text = "set" then
         A_Var.Kind := Set_Kind;

         if Node.F_Args.Children_Count /= 1 then
            Error ("missing parameter for set");
         end if;

         if Node.F_Args.Child (1).Text /= "string" then
            Error ("only sets of strings are currently supported");
         end if;
      elsif Node.F_Typ.Text = "map" then
         A_Var.Kind := Map_Kind;

         if Node.F_Args.Children_Count /= 2 then
            Error ("need to specify key and element");
         end if;

         if Node.F_Args.Child (1).Text /= "string"
           or else Node.F_Args.Child (2).Text /= "object"
         then
            Error ("only (string, object) is currently supported for maps");
         end if;
      else
         Error ("unknown var type: '"
                & Node.F_Typ.Text & "', use text or pattern instead");
      end if;

      for A of Node.As_Var.F_Args loop
         A_Var.Args.Append (Build_Arg (A));
      end loop;

      Pop_Entity;

      return A_Var;
   end Build_Variable_Structure;

   function Build_Command_Scope_Structure (Node : Template_Node'Class) return T_Entity is
      Container_Entity : T_Entity := new T_Entity_Type;

      Dummy_Command : Structure.T_Command;
   begin
      Push_Entity (Container_Entity, Node);

      for C of Node.As_Nested_Scope.F_Scope loop
         case C.Kind is
            when Template_Command =>
               Dummy_Command := Build_Command_Structure (Template_Node (C));

            when others =>
               Error
                 ("unexpected node kind for command scope: '"
                  & Node.Kind'Wide_Wide_Image
                  & "'");
         end case;
      end loop;

      Pop_Entity;

      return Container_Entity;
   end Build_Command_Scope_Structure;

   procedure Resolve_Namespace_Names (A_Namespace : Structure.T_Namespace) is
   begin
      for C of A_Namespace.Children_Ordered loop
         if C.all in T_Namespace_Type'Class then
            Resolve_Namespace_Names (T_Namespace (C));
         elsif C.all in T_Module_Type'Class then
            Resolve_Module_Names (Structure.T_Module (C));
         end if;
      end loop;
   end Resolve_Namespace_Names;

   procedure Resolve_Module_Names (A_Module : Structure.T_Module) is
   begin
      for C of A_Module.Node.As_Module.F_Import_Clauses.Children loop
         Push_Error_Location (C);

         if C.Kind = Template_Import then
            declare
               Imported : Structure.T_Module :=
                 Resolve_Module_By_Name (C.As_Import.F_Name.Text);
            begin
               if Imported = null then
                  Error ("can't find module '" & C.As_Import.F_Name.Text & "'");
               end if;

               A_Module.Imported_Modules.Insert (C.As_Import.F_Name.Text, Imported);
            end;
         end if;

         Pop_Error_Location;
      end loop;

      for C of A_Module.Children_Ordered loop
         if C.all in T_Template_Type'Class then
            Resolve_Template_Names (Structure.T_Template (C));
         elsif C.all in T_Command_Type'Class then
            Resolve_Command_Names (Structure.T_Command (C));
         elsif C.all in T_Visitor_Type'Class then
            Resolve_Visitor_Names (Structure.T_Visitor (C));
         end if;
      end loop;
   end Resolve_Module_Names;

   function Get_Static_Entity_By_Name (Current_Scope : T_Entity; Name : Selector) return Structure.T_Entity is

      function Get_Visible_Entity (An_Entity : T_Entity; Name : Text_Type) return Structure.T_Entity is
      begin
         if An_Entity.all in T_Module_Type then
            declare
               Potential_Entity : Structure.T_Entity;
            begin
               if T_Module_Type (An_Entity.all).Children_Indexed.Contains (Name) then
                  Potential_Entity := T_Module_Type (An_Entity.all).Children_Indexed.Element (Name);
               end if;

               for M of T_Module_Type (An_Entity.all).Imported_Modules loop
                  if M.Children_Indexed.Contains (Name) then
                     if Potential_Entity /= null then
                        Error ("entity name ambiguous, multiple import clauses hiding");
                     else
                        Potential_Entity := M.Children_Indexed.Element (Name);
                     end if;
                  end if;
               end loop;

               return Potential_Entity;
            end;
         elsif An_Entity.Parent /= null then
            return Get_Visible_Entity (An_Entity.Parent, Name);
         else
            return null;
         end if;
      end Get_Visible_Entity;

      Extending_Module : Structure.T_Module;
      Result : Structure.T_Entity;
   begin
      Push_Error_Location (Name);

      if not Name.F_Left.Is_Null then
         Extending_Module := Resolve_Module_By_Name
           (Name.F_Left.Text);

         if Extending_Module = null then
            Error ("module '" & Name.F_Left.Text & "' not found");
         end if;

         if Extending_Module.Children_Indexed.Contains
           (Name.F_Right.Text)
         then
            Result := Extending_Module.Children_Indexed.Element
              (Name.F_Right.Text);
         end if;
      else
         Result := Get_Visible_Entity (Current_Scope, Name.Text);
      end if;

      Pop_Error_Location;

      return Result;
   end Get_Static_Entity_By_Name;

   function Get_Template_Or_Visitor_By_Name (Current_Scope : T_Entity; Name : Selector) return Structure.T_Entity is
      An_Entity : T_Entity;
   begin
      An_Entity := Get_Static_Entity_By_Name
        (Current_Scope,
         Name);

      if An_Entity = null then
         Error ("can't find reference to '" & Name.Text & "'");
      end if;

      if An_Entity.all not in T_Template_Type'Class
        and then An_Entity.all not in T_Visitor_Type'Class
      then
         Error ("expected visitor or template name");
      end if;

      return An_Entity;
   end Get_Template_Or_Visitor_By_Name;

   procedure Resolve_Template_Names (A_Template : Structure.T_Template) is
      An_Entity : T_Entity;
   begin
      if not A_Template.Node.As_Template.F_Extending.Is_Null then
         Push_Error_Location (A_Template.Node.As_Template.F_Extending);

         An_Entity :=
           Get_Static_Entity_By_Name
             (A_Template.Parent, A_Template.Node.As_Template.F_Extending);

         if An_Entity = null then
            Error ("template '" & A_Template.Node.As_Template.F_Extending.Text & "' not found");
         end if;

         if An_Entity.all not in T_Template_Type'Class then
            Error ("expected template name");
         end if;

         A_Template.Extends := Structure.T_Template (An_Entity);
         Pop_Error_Location;
      end if;
   end Resolve_Template_Names;

   procedure Resolve_Command_Names (A_Command : T_Command) is
   begin
      if A_Command.Template_Section /= null then
         case A_Command.Template_Section.Node.F_Actions.Kind is
            when Template_Template_Call =>
               declare
                  An_Operation : Template_Call :=
                    A_Command.Template_Section.Node.F_Actions.As_Template_Call;
               begin
                  if not An_Operation.F_Name.Is_Null then
                     Push_Error_Location (A_Command.Template_Section.Node);

                     if An_Operation.F_Name.Is_Null then
                        if A_Command.Template_Section.Node.Kind = Template_Wrap_Section then
                           Error ("template instances can only be weaved, not wrapped");
                        end if;
                     else
                        if  An_Operation.F_Name.Text = "null" then
                           --  In this case, we're creating a template cancellation
                           --  clause.

                           if A_Command.Template_Section.Node.Kind = Template_Weave_Section then
                              Error ("null template only allowed on wrap clauses");
                           end if;

                           A_Command.Template_Section.Is_Null := True;
                        else
                           A_Command.Template_Section.Call_Reference := Get_Template_Or_Visitor_By_Name
                             (A_Command.Parent,
                              An_Operation.F_Name);
                        end if;
                     end if;

                     for A of An_Operation.F_Args loop
                        A_Command.Template_Section.Args.Append (Build_Arg (A));
                     end loop;

                     Pop_Error_Location;
                  end if;
               end;

            when Template_Traverse_Into =>
               A_Command.Template_Section.A_Visit_Action := Into;

            when Template_Traverse_Over =>
               A_Command.Template_Section.A_Visit_Action := Over;

            when others =>
               Error ("unrecognized template action: "
                      & A_Command.Template_Section.Node.F_Actions.Kind'Wide_Wide_Image);

         end case;
      end if;

      if A_Command.Nested_Actions /= null then
         for C of A_Command.Nested_Actions.Children_Ordered loop
            if C.all in T_Command_Type'Class then
               Resolve_Command_Names (Structure.T_Command (C));
            end if;
         end loop;
      end if;

      if A_Command.Else_Actions /= null then
         if A_Command.Else_Actions.all in T_Command_Type then
            Resolve_Command_Names (Structure.T_Command (A_Command.Else_Actions));
         else
            for C of A_Command.Else_Actions.Children_Ordered loop
               if C.all in T_Command_Type'Class then
                  Resolve_Command_Names (Structure.T_Command (C));
               end if;
            end loop;
         end if;
      end if;
   end Resolve_Command_Names;

   procedure Resolve_Visitor_Names (A_Visitor : Structure.T_Visitor) is
   begin
      for C of A_Visitor.Children_Ordered loop
         if C.all in T_Command_Type'Class then
            Resolve_Command_Names (Structure.T_Command (C));
         end if;
      end loop;
   end Resolve_Visitor_Names;

   function Build_Expr (Node : Template_Node'Class) return T_Expr is
      Expr : T_Expr := new T_Expr_Type (Node.Kind);
      Parent : T_Expr;
   begin
      if Entity_Stack.Length > 0
        and then Entity_Stack.Last_Element.all in T_Expr_Type
      then
         Parent := T_Expr (Entity_Stack.Last_Element);
      end if;

      Push_Entity (Expr, Node);

      case Node.Kind is
         when Template_Match_Capture =>
            Expr.Match_Expr := Build_Expr (Node.As_Match_Capture.F_Expression);

         when Template_Selector =>
            Expr.Selector_Left := Build_Expr (Node.As_Selector.F_Left);
            Expr.Selector_Right := Build_Expr (Node.As_Selector.F_Right);

            if Expr.Selector_Left.Kind in Template_All_Expr | Template_Fold_Expr then
               if Parent = null or else Parent.Kind /= Template_Selector then
                  Error ("all () needs to be selected on an object");
               else
                  Parent.Selector_Left_Expansion := Expr.Selector_Left;
               end if;
            elsif Expr.Selector_Right.Kind in Template_All_Expr | Template_Fold_Expr then
               Expr.Selector_Left_Expansion := Expr.Selector_Right;
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
            Analyze_Replace_String (Node, Expr.Str);

         when Template_Call_Expr =>
            Expr.Called := Build_Expr (Node.As_Call_Expr.F_Called);

            for A of Node.As_Call_Expr.F_Args loop
               Expr.Args.Append (Build_Arg (A));
            end loop;

         when Template_Lambda_Expr =>
            Expr.Lambda_Expression := Build_Expr (Node.As_Lambda_Expr.F_Expression);

         when Template_New_Expr =>
            Expr.Has_New := True;

         when Template_At_Ref =>
            null;

         when Template_Qualified_Match =>
            Expr.Qualified_Match_Expr := Build_Expr (Node.As_Qualified_Match.F_Rhs);

         when Template_Fold_Expr =>
            Expr.Default := Build_Expr (Node.As_Fold_Expr.F_Default);
            Expr.Combine := Build_Expr (Node.As_Fold_Expr.F_Combine);

         when Template_All_Expr =>
            Expr.All_Match := Build_Expr (Node.As_All_Expr.F_Expression);

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
      Arg.Name_Node := Node.As_Argument.F_Name;

      if not Node.As_Argument.F_Name.Is_Null then
         Arg.Name := To_Unbounded_Text (Node.As_Argument.F_Name.Text);
      end if;

      Arg.Expr := Build_Expr (Node.As_Argument.F_Value);

      return Arg;
   end Build_Arg;

   Expression_Unit_Number : Integer := 1;

   procedure Analyze_Replace_String
     (Node   : Template_Node'Class;
      Result : in out Processed_String_Vector.Vector)
   is
      Str : constant Text_Type := Remove_Quotes (Node.Text);
      Context : constant Analysis_Context := Node.Unit.Context;

      Next_Index : Integer := Str'First;

      Current : Integer := Str'First;

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
   begin
      Prev_Error := Error_Callback;
      Error_Callback := On_Error'Unrestricted_Access;

      if Str = "" then
         Error_Callback := Prev_Error;

         return;
      end if;

      while Current <= Str'Last loop
         if Str (Current) = '\' then
            if Current /= Str'First then
               Result.Append
                 ((Str_Kind, 0, 0, To_Unbounded_Text (Str (Next_Index .. Current - 1))));
            end if;

            Current := Current + 1;

            if Str (Current) = 'e' then
               Current := Current + 1;

               if Str (Current) = '<' then
                  Next_Index := Current;

                  while Next_Index < Str'Last and then Str (Next_Index) /= '>' loop
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

                     Result.Append ((Expr_Kind, 0, 0, Build_Expr (Expression_Unit.Root)));
                  end;

                  Current := Next_Index + 1;
                  Next_Index := Current;
               else
                  Result.Append ((Str_Kind, 0, 0, To_Unbounded_Text (Str (Current - 1 .. Current))));
                  Next_Index := Current;
                  Current := Current + 1;
               end if;
            elsif Str (Current) = 'n' then
               Result.Append ((Str_Kind, 0, 0, To_Unbounded_Text (To_Text (String'(1 => ASCII.LF)))));
               Current := Current + 1;
               Next_Index := Current;
            elsif Str (Current) in '0' .. '9' then
               Next_Index := Current;

               while Next_Index < Str'Last and then Str (Next_Index) in '0' .. '9' loop
                  Next_Index := Next_Index + 1;
               end loop;

               if Str (Next_Index) not in '0' .. '9' then
                  Next_Index := Next_Index - 1;
               end if;

               declare
                  Group_Value : Natural :=
                    Natural'Wide_Wide_Value (Str (Current .. Next_Index));
               begin
                  Result.Append ((Group_Kind, 0, 0, Group_Value));
               end;

               Current := Next_Index + 1;
               Next_Index := Current;
            elsif Str (Current) = '\' then
               Result.Append ((Str_Kind, 0, 0, To_Unbounded_Text ("\")));
               Next_Index := Current + 1;
               Current := Current + 1;
            else
               Next_Index := Current;
               Current := Current + 1;
            end if;
         else
            Current := Current + 1;
         end if;
      end loop;

      if Next_Index <= Str'Last then
         -- Add the end of the text to the result

         Result.Append ((Str_Kind, 0, 0, To_Unbounded_Text (Str (Next_Index .. Str'Last))));
      end if;
   end Analyze_Replace_String;

end Wrapping.Semantic.Analysis;
