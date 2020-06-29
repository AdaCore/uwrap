with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Containers; use Ada.Containers;

with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Wrapping.Utils; use Wrapping.Utils;

package body Wrapping.Semantic.Structure is

   procedure Add_Child (Parent, Child : access T_Entity_Type'Class) is
   begin
      Child.Parent := T_Entity (Parent);

      if Parent.Children_Ordered.Length > 0 then
         Parent.Children_Ordered.Last_Element.Next := T_Entity (Child);
         Child.Prev := Parent.Children_Ordered.Last_Element;
      end if;

      Parent.Children_Ordered.Append (T_Entity (Child));
   end Add_Child;

   procedure Add_Child (Parent, Child : access T_Entity_Type'Class; Name_Node : Template_Node'Class) is
   begin
      Add_Child (Parent, Child);
      Parent.Children_Indexed.Insert (Name_Node.Text, T_Entity (Child));
   end Add_Child;

   procedure Add_Child (Parent, Child : access T_Entity_Type'Class; Name : Text_Type) is
   begin
      Add_Child (Parent, Child);
      Parent.Children_Indexed.Insert (Name, T_Entity (Child));
   end Add_Child;

   function Full_Name (An_Entity : T_Entity_Type) return Text_Type is
   begin
      if An_Entity.Parent = null then
         return "";
      else
         return An_Entity.Parent.Full_Name;
      end if;
   end Full_Name;

   function Find_Visible_Entity (An_Entity : T_Entity_Type'Class; Name : Text_Type) return T_Entity
   is
   begin
      if An_Entity.Children_Indexed.Contains (Name) then
         return An_Entity.Children_Indexed.Element (Name);
      elsif An_Entity.Parent /= null then
         return Find_Visible_Entity (An_Entity.Parent.all, Name);
      else
         return null;
      end if;
   end Find_Visible_Entity;

   function Full_Name (An_Entity : T_Named_Entity_Type) return Text_Type is
   begin
      if An_Entity.Parent = null then
         if An_Entity.Name_Node.Is_Null then
            return "";
         else
            return An_Entity.Name_Node.Text;
         end if;
      else
         declare
            Parent_Text : Text_Type := An_Entity.Parent.Full_Name;
         begin
            if Parent_Text = "" then
               if An_Entity.Name_Node.Is_Null then
                  return "";
               else
                  return An_Entity.Name_Node.Text;
               end if;
            else
               return Parent_Text & "." & An_Entity.Name_Node.Text;
            end if;
         end;
      end if;
   end Full_Name;

   function Resolve_Module_By_Name (Name : Text_Type) return T_Module is
      Result : T_Entity;
      A_Namespace : T_Namespace;
      A_Suffix : Text_Type := Suffix (Name);
   begin
      A_Namespace := Get_Namespace_Prefix (Name);

      if A_Namespace.Children_Indexed.Contains (A_Suffix) then
         Result := A_Namespace.Children_Indexed.Element (A_Suffix);

         if Result.all in T_Module_Type'Class then
            return T_Module (Result);
         end if;
      end if;

      return null;
   end Resolve_Module_By_Name;

   function Full_Name (An_Entity : T_Module_Type) return Text_Type is
   begin
      if An_Entity.Parent = null then
         return To_Text (An_Entity.Name);
      else
         return An_Entity.Parent.Full_Name & "." & To_Text (An_Entity.Name);
      end if;
   end Full_Name;

   function Get_Variable_For_Index (An_Entity : T_Module_Type; Index : Positive) return T_Var
   is
   begin
      return An_Entity.Variables_Ordered (Index);
   end Get_Variable_For_Index;

   function Get_Component (An_Entity : T_Module_Type; Name : Text_Type) return T_Entity is
   begin
      return An_Entity.Children_Indexed (Name);
   end Get_Component;

   function Instance_Of (Child, Parent : T_Template) return Boolean is
   begin
      if Child = Parent then
         return True;
      elsif Child.Extends = null then
         return False;
      else
         return Instance_Of (Child.Extends, Parent);
      end if;
   end Instance_Of;

   function Has_Variable (A_Template : T_Template_Type; Name : Text_Type) return Boolean is
   begin
      if A_Template.Variables_Indexed.Contains (Name) then
         return True;
      elsif A_Template.Extends /= null then
         return Has_Variable (A_Template.Extends.all, Name);
      else
         return False;
      end if;
   end Has_Variable;

   function Get_Variable_For_Index (A_Template : T_Template_Type; Index : Positive) return T_Var
   is
      Result : T_Var;

      procedure Recursive_Search (A_Template : T_Template_Type'Class; Start_Offset : in out Integer) is
      begin
         if A_Template.Extends /= null then
            Recursive_Search (A_Template.Extends.all, Start_Offset);
         end if;

         if Result /= null then
            return;
         end if;

         if A_Template.Variables_Ordered.Length > 0
           and then Index in Start_Offset + 1 .. Start_Offset + Integer (A_Template.Variables_Ordered.Length)
         then
            Result := A_Template.Variables_Ordered.Element (Index - Start_Offset);
         else
            Start_Offset := Start_Offset + Integer (A_Template.Variables_Ordered.Length);
         end if;

      end Recursive_Search;

      Start : Integer := 0;
   begin
      Recursive_Search (A_Template, Start);

      return Result;
   end Get_Variable_For_Index;

   function Get_Component (A_Template : T_Template_Type; Name : Text_Type) return T_Entity is
   begin
      if A_Template.Children_Indexed.Contains (Name) then
         return A_Template.Children_Indexed.Element (Name);
      elsif A_Template.Extends = null then
         return null;
      else
         return A_Template.Extends.Get_Component (Name);
      end if;
   end Get_Component;

   function Get_Namespace_Prefix (Full_Name : Text_Type; Create_If_Null : Boolean := False) return T_Namespace is
      First, Dot : Integer;
      Tentative : T_Entity;
      Current : T_Namespace := Wrapping.Semantic.Analysis.Root;
      New_Namespace : T_Namespace;
   begin
      First := Full_Name'First;

      loop
         Dot := Index (Full_Name (First .. Full_Name'Last), ".");

         if Dot = 0 then
            return Current;
         else
            declare
               Section : Text_Type := Full_Name (First .. Dot - 1);
            begin

               if Current.Children_Indexed.Contains (Section) then
                  Tentative := Current.Children_Indexed.Element (Section);

                  if Tentative.all not in T_Namespace_Type then
                     Error ("Expected namespace");
                  else
                     Current := T_Namespace (Tentative);
                  end if;
               elsif Create_If_Null then
                  New_Namespace := new T_Namespace_Type;
                  Add_Child (Current, New_Namespace, Section);
                  Current := New_Namespace;
               else
                  return null;
               end if;
            end;
         end if;

         First := Dot + 1;
      end loop;
   end Get_Namespace_Prefix;

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

   procedure Resolve_Names (An_Entity : access T_Entity_Type) is
   begin
      for C of An_Entity.Children_Ordered loop
         C.Resolve_Names;
      end loop;
   end Resolve_Names;

   overriding
   procedure Resolve_Names (An_Entity : access T_Template_Type) is
      Extending : T_Entity;
   begin
      T_Entity_Type (An_Entity.all).Resolve_Names;

      if not An_Entity.Node.As_Template.F_Extending.Is_Null then
         Push_Error_Location (An_Entity.Node.As_Template.F_Extending);

         Extending :=
           Get_Static_Entity_By_Name
             (An_Entity.Parent, An_Entity.Node.As_Template.F_Extending);

         if Extending = null then
            Error ("template '" & An_Entity.Node.As_Template.F_Extending.Text & "' not found");
         end if;

         if Extending.all not in T_Template_Type'Class then
            Error ("expected template name");
         end if;

         An_Entity.Extends := T_Template (Extending);
         Pop_Error_Location;
      end if;
   end Resolve_Names;

   overriding
   procedure Resolve_Names (An_Entity : access T_Command_Type) is
   begin
      T_Entity_Type (An_Entity.all).Resolve_Names;

      if An_Entity.Template_Section /= null then
         case An_Entity.Template_Section.Node.F_Actions.Kind is
            when Template_Template_Call =>
               declare
                  An_Operation : Template_Call :=
                    An_Entity.Template_Section.Node.F_Actions.As_Template_Call;
               begin
                  if not An_Operation.F_Name.Is_Null then
                     Push_Error_Location (An_Entity.Template_Section.Node);

                     if An_Operation.F_Name.Is_Null then
                        if An_Entity.Template_Section.Node.Kind = Template_Wrap_Section then
                           Error ("template instances can only be weaved, not wrapped");
                        end if;
                     else
                        if  An_Operation.F_Name.Text = "null" then
                           --  In this case, we're creating a template cancellation
                           --  clause.

                           if An_Entity.Template_Section.Node.Kind = Template_Weave_Section then
                              Error ("null template only allowed on wrap clauses");
                           end if;

                           An_Entity.Template_Section.Is_Null := True;
                        else
                           An_Entity.Template_Section.Call_Reference :=
                             Get_Template_Or_Visitor_By_Name
                               (An_Entity.Parent,
                                An_Operation.F_Name);
                        end if;
                     end if;

                     Pop_Error_Location;
                  end if;
               end;

            when Template_Traverse_Into =>
               An_Entity.Template_Section.A_Visit_Action := Into;

            when Template_Traverse_Over =>
               An_Entity.Template_Section.A_Visit_Action := Over;

            when others =>
               Error ("unrecognized template action: "
                      & An_Entity.Template_Section.Node.F_Actions.Kind'Wide_Wide_Image);

         end case;
      end if;
   end Resolve_Names;

   overriding
   procedure Resolve_Names (An_Entity : access T_Module_Type) is
   begin
      T_Entity_Type (An_Entity.all).Resolve_Names;

      for C of An_Entity.Node.As_Module.F_Import_Clauses.Children loop
         Push_Error_Location (C);

         if C.Kind = Template_Import then
            declare
               Imported : Structure.T_Module :=
                 Resolve_Module_By_Name (C.As_Import.F_Name.Text);
            begin
               if Imported = null then
                  Error ("can't find module '" & C.As_Import.F_Name.Text & "'");
               end if;

               An_Entity.Imported_Modules.Insert (C.As_Import.F_Name.Text, Imported);
            end;
         end if;

         Pop_Error_Location;
      end loop;
   end Resolve_Names;

   overriding
   procedure Resolve_Names (An_Entity : access T_Expr_Type) is
   begin
      T_Entity_Type (An_Entity.all).Resolve_Names;

      --  TODO: There are a few cases where names can be resolved statically,
      --  e.g. static references in identifiers
   end Resolve_Names;

   overriding
   procedure Resolve_Names (An_Entity : access T_Create_Tree_Type) is
   begin
      T_Entity_Type (An_Entity.all).Resolve_Names;

      Push_Error_Location (An_Entity.Node);

      if not An_Entity.Node.As_Create_Template_Tree.F_Root.Is_Null then
         An_Entity.A_Template :=
           T_Template
             (Get_Static_Entity_By_Name
                (T_Entity (An_Entity),
                 An_Entity.Node.As_Create_Template_Tree.F_Root.F_Name));

         if An_Entity.A_Template = null then
            Error ("'" & An_Entity.Node.As_Create_Template_Tree.F_Root.Text & " not found.");
         end if;
      end if;

      Pop_Error_Location;
   end Resolve_Names;

end Wrapping.Semantic.Structure;
