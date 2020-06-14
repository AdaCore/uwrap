with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with Langkit_Support; use Langkit_Support;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Libtemplatelang.Common; use Libtemplatelang.Common;

with Wrapping.Utils; use Wrapping.Utils;
with Langkit_Support.Text; use Langkit_Support.Text;

package body Wrapping.Semantic.Analysis is

   --  TODO: Since names are resolved in the runtime analysis, we probably
   --  don't need lexical scopes here...
   Lexical_Scope_Stack : Entity_Vectors.Vector;

   Entity_Stack : Entity_Vectors.Vector;

   function Build_Template_Structure (Node : Template_Node) return Structure.Template;
   function Build_Module_Structure (Node : Template_Node; Module_Name : Text_Type) return Structure.Module;
   function Build_Command_Structure (Node : Template_Node'Class) return Structure.Command;
   function Build_Visitor_Structure (Node : Template_Node) return Structure.Visitor;
   function Build_Variable_Structure (Node : Template_Node) return Structure.Var;
   function Build_Command_Scope_Structure (Node : Template_Node'Class) return Entity;

   procedure Resolve_Template_Names (A_Template : Structure.Template);
   procedure Resolve_Module_Names (A_Module : Structure.Module);
   procedure Resolve_Command_Names (A_Command : Structure.Command);
   procedure Resolve_Visitor_Names (A_Visitor : Structure.Visitor);
   procedure Resolve_Namespace_Names (A_Namespace : Structure.Namespace);
   function Get_Static_Entity_By_Name (Current_Scope : Entity; Name : Selector) return Structure.Entity;

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
      File_Module : Structure.Module;
   begin
      Entity_Stack.Append (Entity (Root));
      File_Module := Build_Module_Structure (Unit.Root, To_Text (Name));
      Entity_Stack.Delete_Last;
   end Load_Module;

   procedure Push_Error_Location (Node : Template_Node'Class) is
   begin
      Push_Error_Location
        (Node.Unit.Get_Filename,
         (Node.Sloc_Range.Start_Line, Node.Sloc_Range.Start_Column));
   end Push_Error_Location;

   procedure Push_Entity (An_Entity : access Entity_Type'Class; Node : Template_Node'Class) is
   begin
      Add_Child (Entity_Stack.Last_Element, Entity (An_Entity));
      An_Entity.Node := Template_Node (Node);
      Entity_Stack.Append (Entity (An_Entity));
   end Push_Entity;

   procedure Push_Named_Entity
     (An_Entity : access Entity_Type'Class;
      Node      : Template_Node'Class;
      Name      : Text_Type) is
   begin
      Add_Child (Entity_Stack.Last_Element, Entity (An_Entity), Name);
      An_Entity.Node := Template_Node (Node);
      Entity_Stack.Append (Entity (An_Entity));
   end Push_Named_Entity;

   procedure Push_Named_Entity (An_Entity : access Named_Entity_Type'Class; Node : Template_Node'Class; Name_Node : Template_Node'Class) is
   begin
      Add_Child (Entity_Stack.Last_Element, Entity (An_Entity), Name_Node);
      An_Entity.Node := Template_Node (Node);
      Entity_Stack.Append (Entity (An_Entity));
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
      return Structure.Module
   is
      A_Module : Structure.Module := new Module_Type;
      A_Namespace : Structure.Namespace;

      Dummy_Command : Structure.Command;

      Program_Node : Template_Node;
   begin
      A_Namespace := Get_Namespace_Prefix (Module_Name, True);

      --  The module needs to be stacked manually, as it's not a child of
      --  the currently stacked entity (the root node) but of the namespace.
      Add_Child (A_Namespace, A_Module, Suffix (Module_Name));
      A_Module.Node := Node;
      Entity_Stack.Append (Entity (A_Module));

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

   function Build_Template_Structure (Node : Template_Node) return Structure.Template is
      A_Template : Structure.Template := new Template_Type;
   begin
      Push_Named_Entity (A_Template, Node, Node.As_Template.F_Name);

      for C of Node.As_Template.F_Definition loop
         case C.Kind is
            when Template_Var =>
               A_Template.Variables_Ordered.Append
                 (Build_Variable_Structure (Template_Node (C)));
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

   function Build_Command_Structure (Node : Template_Node'Class) return Structure.Command is
      A_Command : Structure.Command := new Command_Type;

      function Visit (Node : Template_Node'Class) return Visit_Status is
      begin
         case Node.Kind is
            when Template_Match_Clause =>
               A_Command.Match_Expression := Template_Node
                 (Node.As_Match_Clause.F_Expression);

               return Over;
            when Template_Wrap_Clause | Template_Weave_Clause =>
               if Node.Kind = Template_Wrap_Clause then
                  A_Command.Template_Clause := new Wrap_Type;
               else
                  A_Command.Template_Clause := new Weave_Type;
               end if;

               A_Command.Template_Clause.Node := Node.As_Template_Clause;

               return Over;
            when Template_Nested_Scope =>
               A_Command.Nested_Actions :=
                 Build_Command_Scope_Structure (Node.As_Nested_Scope);

               return Over;
            when Template_Else_Clause =>
               case Node.As_Else_Clause.F_Actions.Kind is
                  when Template_Command =>
                     A_Command.Else_Actions := Entity (Build_Command_Structure (Node.As_Else_Clause.F_Actions));

                  when Template_Nested_Scope =>
                     A_Command.Else_Actions := Build_Command_Scope_Structure (Node.As_Else_Clause.F_Actions);

                  when others =>
                     Error
                       ("unexpected node kind for command else: '"
                        & Node.As_Else_Clause.F_Actions.Kind'Wide_Wide_Image
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

   function Build_Visitor_Structure (Node : Template_Node) return Structure.Visitor is
      A_Visitor : Semantic.Structure.Visitor := new Visitor_Type;
      Program_Node : Template_Node;
      Dummy_Command : Structure.Command;
   begin
      Push_Named_Entity (A_Visitor, Node, Node.As_Visitor.F_Name);

      for A of Node.As_Visitor.F_Args loop
         declare
            A_Var : Semantic.Structure.Var := new Var_Type;
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

   function Build_Variable_Structure (Node : Template_Node) return Structure.Var is
      A_Var : Structure.Var := new Var_Type;
   begin
      Push_Named_Entity (A_Var, Node, Node.As_Var.F_Name);

      if Node.As_Var.F_Typ.Text = "text" then
         A_Var.Kind := Text_Kind;

         if Node.As_Var.F_Args.Children_Count /= 0 then
            Error ("no argument expected for text var");
         end if;
      elsif Node.As_Var.F_Typ.Text = "pattern" then
         A_Var.Kind := Pattern_Kind;

         if Node.As_Var.F_Args.Children_Count /= 1 then
            Error ("missing parameter for pattern");
         end if;
      else
         Error ("unknown var type: '"
                & Node.As_Var.F_Typ.Text & "', use text or pattern instead");
      end if;

      A_Var.Args := Node.As_Var.F_Args;

      Pop_Entity;

      return A_Var;
   end Build_Variable_Structure;

   function Build_Command_Scope_Structure (Node : Template_Node'Class) return Entity is
      Container_Entity : Entity := new Entity_Type;

      Dummy_Command : Structure.Command;
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

   procedure Resolve_Namespace_Names (A_Namespace : Structure.Namespace) is
   begin
      for C of A_Namespace.Children_Ordered loop
         if C.all in Namespace_Type'Class then
            Resolve_Namespace_Names (Namespace (C));
         elsif C.all in Module_Type'Class then
            Resolve_Module_Names (Structure.Module (C));
         end if;
      end loop;
   end Resolve_Namespace_Names;

   procedure Resolve_Module_Names (A_Module : Structure.Module) is
   begin
      for C of A_Module.Node.As_Module.F_Import_Clauses.Children loop
         Push_Error_Location (C);

         if C.Kind = Template_Import then
            declare
               Imported : Structure.Module :=
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
         if C.all in Template_Type'Class then
            Resolve_Template_Names (Structure.Template (C));
         elsif C.all in Command_Type'Class then
            Resolve_Command_Names (Structure.Command (C));
         elsif C.all in Visitor_Type'Class then
            Resolve_Visitor_Names (Structure.Visitor (C));
         end if;
      end loop;
   end Resolve_Module_Names;

   function Get_Static_Entity_By_Name (Current_Scope : Entity; Name : Selector) return Structure.Entity is

      function Get_Visible_Entity (An_Entity : Entity; Name : Text_Type) return Structure.Entity is
      begin
         if An_Entity.all in Module_Type then
            declare
               Potential_Entity : Structure.Entity;
            begin
               if Module_Type (An_Entity.all).Children_Indexed.Contains (Name) then
                  Potential_Entity := Module_Type (An_Entity.all).Children_Indexed.Element (Name);
               end if;

               for M of Module_Type (An_Entity.all).Imported_Modules loop
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

      Extending_Module : Structure.Module;
      Result : Structure.Entity;
   begin
      Push_Error_Location (Name);

      if not Name.F_Left.Is_Null then
         Extending_Module := Resolve_Module_By_Name
           (Name.F_Left.Text);

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

   procedure Resolve_Template_Names (A_Template : Structure.Template) is
      An_Entity : Entity;
   begin
      if not A_Template.Node.As_Template.F_Extending.Is_Null then
         Push_Error_Location (A_Template.Node.As_Template.F_Extending);

         An_Entity :=
           Get_Static_Entity_By_Name
             (A_Template.Parent, A_Template.Node.As_Template.F_Extending);

         if An_Entity = null then
            Error ("template '" & A_Template.Node.As_Template.F_Extending.Text & "' not found");
         end if;

         if An_Entity.all not in Template_Type'Class then
            Error ("expected template name");
         end if;

         A_Template.Extends := Structure.Template (An_Entity);
         Pop_Error_Location;
      end if;
   end Resolve_Template_Names;

   procedure Resolve_Command_Names (A_Command : Structure.Command) is
   begin
      if A_Command.Template_Clause /= null then
         case A_Command.Template_Clause.Node.F_Action.Kind is
            when Template_Template_Operation =>
               declare
                  An_Operation : Template_Operation :=
                    A_Command.Template_Clause.Node.F_Action.As_Template_Operation;
               begin
                  if not An_Operation.F_Entity.Is_Null then
                     if An_Operation.F_Entity.Kind in Template_Tree_Reference then
                        A_Command.Template_Clause.Is_All := True;
                     end if;

                     A_Command.Template_Clause.Target_Object :=
                       Template_Node (An_Operation.F_Entity.F_Value);
                  end if;

                  if not An_Operation.F_Call.Is_Null then
                     Push_Error_Location (A_Command.Template_Clause.Node);

                     if An_Operation.F_Call.F_Name.Is_Null then
                        if A_Command.Template_Clause.Node.Kind = Template_Wrap_Clause then
                           Error ("template instances can only be weaved, not wrapped");
                        end if;
                     else
                        A_Command.Template_Clause.Call_Reference := Get_Static_Entity_By_Name
                          (A_Command.Parent,
                           An_Operation.F_Call.F_Name);

                        if A_Command.Template_Clause.Call_Reference = null then
                           Error ("can't find reference to '" & An_Operation.F_Call.F_Name.Text & "'");
                        end if;

                        if A_Command.Template_Clause.Call_Reference.all not in Template_Type'Class
                          and then A_Command.Template_Clause.Call_Reference.all not in Visitor_Type'Class
                        then
                           Error ("expected visitor or template name");
                        end if;
                     end if;

                     A_Command.Template_Clause.Arguments := An_Operation.F_Call.F_Args;
                     Pop_Error_Location;
                  end if;
               end;

            when Template_Traverse_Into =>
               A_Command.Template_Clause.A_Visit_Action := Into;

            when Template_Traverse_Over =>
               A_Command.Template_Clause.A_Visit_Action := Over;

            when others =>
               Error ("unrecognized template action: "
                      & A_Command.Template_Clause.Node.F_Action.Kind'Wide_Wide_Image);

         end case;
      end if;

      if A_Command.Nested_Actions /= null then
         for C of A_Command.Nested_Actions.Children_Ordered loop
            if C.all in Command_Type'Class then
               Resolve_Command_Names (Structure.Command (C));
            end if;
         end loop;
      end if;

      if A_Command.Else_Actions /= null then
         if A_Command.Else_Actions.all in Command_Type then
            Resolve_Command_Names (Structure.Command (A_Command.Else_Actions));
         else
            for C of A_Command.Else_Actions.Children_Ordered loop
               if C.all in Command_Type'Class then
                  Resolve_Command_Names (Structure.Command (C));
               end if;
            end loop;
         end if;
      end if;
   end Resolve_Command_Names;

   procedure Resolve_Visitor_Names (A_Visitor : Structure.Visitor) is
   begin
      for C of A_Visitor.Children_Ordered loop
         if C.all in Command_Type'Class then
            Resolve_Command_Names (Structure.Command (C));
         end if;
      end loop;
   end Resolve_Visitor_Names;

end Wrapping.Semantic.Analysis;
