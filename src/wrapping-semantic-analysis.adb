with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

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
   function Build_Module_Structure (Node : Template_Node; Is_File_Root : Boolean := False) return Structure.Module;
   function Build_Command_Structure (Node : Template_Node) return Structure.Command;
   function Build_Command_Function_Structure (Node : Template_Node) return Structure.Command_Function;
   function Build_Variable_Structure (Node : Template_Node) return Structure.Var;
   function Build_Command_Scope_Structure (Node : Template_Node'Class) return Entity;

   procedure Resolve_Template_Names (A_Template : Structure.Template);
   procedure Resolve_Module_Names (A_Module : Structure.Module);
   procedure Resolve_Command_Names (A_Command : Structure.Command);
   function Get_Template_By_Name (Current_Scope : Entity; Name : Dotted_Name) return Structure.Template;

   procedure Analyze (Unit : Analysis_Unit);

   package Unit_Vectors is new Ada.Containers.Vectors (Positive, Analysis_Unit);
   Unit_List : Unit_Vectors.Vector;

   procedure Analyze is
      Context : constant Analysis_Context := Create_Context;

      Unit : Analysis_Unit;
   begin
      for File of Files loop
         Unit := Get_From_File (Context, File);

         if Has_Diagnostics (Unit) then
            for D of Libtemplatelang.Analysis.Diagnostics (Unit) loop
               Put_Line (File & ":" & To_Pretty_String (D));
            end loop;
         end if;

         Unit_List.Append (Unit);
      end loop;

      for U of Unit_List loop
         Analyze (U);
      end loop;

      Resolve_Module_Names (Root_Module);
   end Analyze;

   procedure Analyze (Unit : Analysis_Unit) is
      File_Module : Structure.Module;
   begin
      Entity_Stack.Append (Entity (Root_Module));
      File_Module := Build_Module_Structure (Unit.Root, True);
      Root_Module.Modules_Ordered.Append (File_Module);
      Entity_Stack.Delete_Last;
   end Analyze;

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

   function Build_Module_Structure (Node : Template_Node; Is_File_Root : Boolean := False) return Structure.Module is
      A_Module : Structure.Module := new Module_Type;

      Dummy_Command : Structure.Command;

      Program_Node : Template_Node;
   begin
      if Is_File_Root then
         Push_Entity (A_Module, Node);
         Program_Node := Template_Node (Node.As_File.F_Program);
      else
         Push_Named_Entity (A_Module, Node, Node.As_Module.F_Name);
         Program_Node := Template_Node (Node.As_Module.F_Program);
      end if;

      for C of Program_Node.Children loop
         case C.Kind is
            when Template_Module =>
               A_Module.Modules_Ordered.Append
                 (Build_Module_Structure (C));

               A_Module.Modules_Indexed.Insert
                 (C.As_Module.F_Name.Text,
                  A_Module.Modules_Ordered.Last_Element);

               Pop_Entity;
            when Template_Template =>
               A_Module.Templates_Ordered.Append
                 (Build_Template_Structure (C));
               A_Module.Templates_Indexed.Insert
                 (C.As_Template.F_Name.Text,
                  A_Module.Templates_Ordered.Last_Element);

            when Template_Command =>
               Dummy_Command := Build_Command_Structure (C);

            when Template_Command_Function =>
               A_Module.Command_Function_Indexed.Insert
                 (C.As_Command_Function.F_Name.Text,
                  Build_Command_Function_Structure (C));

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

            when Template_Pattern =>
               declare
                  A_Pattern : Structure.Pattern := new Pattern_Type;
               begin
                  Push_Named_Entity (A_Pattern, C, C.As_Pattern.F_Name);
                  A_Pattern.Pattern_Expression := C.As_Pattern.F_Expression;
                  Pop_Entity;
               end;

            when others =>
               Error ("unsupported node for templates: '" & C.Kind'Wide_Wide_Image & "'");
         end case;
      end loop;

      Pop_Entity;

      return A_Template;
   end Build_Template_Structure;

   function Build_Command_Structure (Node : Template_Node) return Structure.Command is
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

               if not Node.As_Template_Clause.F_Target.Is_Null then
                  A_Command.Template_Clause.Target_Object := Node.As_Template_Clause.F_Target;
               end if;

               A_Command.Template_Clause.Arguments := Node.As_Template_Clause.F_Template_Set.F_Args;

               return Over;

            when Template_Apply_Clause =>
               A_Command.Apply_Expression := Template_Node
                 (Node.As_Apply_Clause.F_Expression);

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

   function Build_Command_Function_Structure (Node : Template_Node) return Structure.Command_Function is
   begin
      return null;
   end Build_Command_Function_Structure;

   function Build_Variable_Structure (Node : Template_Node) return Structure.Var is
      A_Var : Structure.Var := new Var_Type;
   begin
      Push_Named_Entity (A_Var, Node, Node.As_Var.F_Name);

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

   procedure Resolve_Module_Names (A_Module : Structure.Module) is
   begin
      if not A_Module.Node.Is_Null and then A_Module.Node.Kind = Template_File then
         for C of A_Module.Node.As_File.F_Use_Clauses.Children loop
            Push_Error_Location (C);

            if C.Kind = Template_Import then
               declare
                  Imported : Structure.Module :=
                    Resolve_Module_By_Name (Root_Module, C.As_Import.F_Name.Text);
               begin
                  if Imported = null then
                     Error ("can't find module '" & C.As_Import.F_Name.Text & "'");
                  end if;

                  A_Module.Imported_Modules.Insert (C.As_Import.F_Name.Text, Imported);
               end;
            end if;

            Pop_Error_Location;
         end loop;
      end if;

      for C of A_Module.Children_Ordered loop
         if C.all in Module_Type'Class then
            Resolve_Module_Names (Structure.Module (C));
         elsif C.all in Template_Type'Class then
            Resolve_Template_Names (Structure.Template (C));
         elsif C.all in Command_Type'Class then
            Resolve_Command_Names (Structure.Command (C));
         end if;
      end loop;
   end Resolve_Module_Names;

   function Get_Template_By_Name (Current_Scope : Entity; Name : Dotted_Name) return Structure.Template is

      function Get_Visible_Template (An_Entity : Entity; Name : Text_Type) return Structure.Template is
      begin
         if An_Entity.all in Module_Type and then
           Module_Type (An_Entity.all).Templates_Indexed.Contains (Name)
         then
            return Module_Type (An_Entity.all).Templates_Indexed.Element (Name);
         elsif An_Entity.Parent /= null then
            return Get_Visible_Template (An_Entity.Parent, Name);
         else
            return null;
         end if;
      end Get_Visible_Template;

      Extending_Module : Structure.Module;
      Result : Structure.Template;
   begin
      Push_Error_Location (Name);

      if not Name.F_Prefix.Is_Null then
         Extending_Module := Resolve_Module_By_Name
           (Root_Module, Name.F_Prefix.Text);

         if Extending_Module.Templates_Indexed.Contains
           (Name.F_Suffix.Text)
         then
            Result := Extending_Module.Templates_Indexed.Element
              (Name.F_Suffix.Text);
         end if;
      else
         Result := Get_Visible_Template (Current_Scope, Name.Text);
      end if;

      if Result = null then
         Error ("can't find template '" & Name.Text & "'");
      end if;

      Pop_Error_Location;

      return Result;
   end Get_Template_By_Name;

   procedure Resolve_Template_Names (A_Template : Structure.Template) is
   begin
      if not A_Template.Node.As_Template.F_Extending.Is_Null then
         A_Template.Extends :=
           Get_Template_By_Name
             (A_Template.Parent, A_Template.Node.As_Template.F_Extending);
      end if;
   end Resolve_Template_Names;

   procedure Resolve_Command_Names (A_Command : Structure.Command) is
   begin
      if A_Command.Template_Clause /= null then
         A_Command.Template_Clause.Template_Reference := Get_Template_By_Name
           (A_Command.Parent,
            A_Command.Template_Clause.Node.F_Template_Set.F_Name);
      end if;

      if A_Command.Nested_Actions /= null then
         for C of A_Command.Nested_Actions.Children_Ordered loop
            if C.all in Command_Type'Class then
               Resolve_Command_Names (Structure.Command (C));
            end if;
         end loop;
      end if;
   end Resolve_Command_Names;

end Wrapping.Semantic.Analysis;
