with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Containers; use Ada.Containers;

with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Wrapping.Utils; use Wrapping.Utils;

package body Wrapping.Semantic.Structure is

   procedure Compute_Closure (Root : T_Entity; Closure : in out Text_Sets.Set);

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
         declare
            Parent_Name : Text_Type := An_Entity.Parent.Full_Name;
         begin
            if Parent_Name = "" then
               return To_Text (An_Entity.Name);
            else
               return Parent_Name & "." & To_Text (An_Entity.Name);
            end if;
         end;
      end if;
   end Full_Name;

   function Get_Component (An_Entity : T_Module_Type; Name : Text_Type) return T_Entity is
   begin
      return An_Entity.Children_Indexed (Name);
   end Get_Component;

   function Instance_Of (Child, Parent : T_Template) return Boolean is
   begin
      if Child = null or else Parent = null then
         return False;
      elsif Child = Parent then
         return True;
      elsif Child.Extends = null then
         return False;
      else
         return Instance_Of (Child.Extends, Parent);
      end if;
   end Instance_Of;

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

   function Get_Template_By_Name (Current_Scope : T_Entity; Name : Selector) return Structure.T_Entity is
      An_Entity : T_Entity;
   begin
      An_Entity := Get_Static_Entity_By_Name (Current_Scope, Name);

      if An_Entity = null then
         Error ("can't find reference to '" & Name.Text & "'");
      end if;

      if An_Entity.all not in T_Template_Type'Class then
         Error ("expected visitor or template name");
      end if;

      return An_Entity;
   end Get_Template_By_Name;

   procedure Resolve_References (An_Entity : access T_Entity_Type) is
   begin
      for C of An_Entity.Children_Ordered loop
         Push_Error_Location (C.Node);
         C.Resolve_References;
         Pop_Error_Location;
      end loop;
   end Resolve_References;

   overriding
   procedure Resolve_References (An_Entity : access T_Template_Type) is
      Extending : T_Entity;
   begin
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

      T_Entity_Type (An_Entity.all).Resolve_References;
   end Resolve_References;

   overriding
   procedure Resolve_References (An_Entity : access T_Template_Call_Type) is
      Name : Text_Type :=
        (if An_Entity.Node.As_Template_Call.F_Name.Is_Null then
            ""
         else An_Entity.Node.As_Template_Call.F_Name.Text);
   begin
      if Name = "" then
         if An_Entity.Parent.Node.Kind /= Template_Weave_Section then
            Error ("self-weaving requires can only be done in weave sections");
         end if;
      elsif Name = "null" then
         --  In this case, we're creating a template cancellation
         --  clause.

         if An_Entity.Parent.Node.Kind /= Template_Wrap_Section then
            Error
              ("null template only allowed on wrap sections, not "
               & An_Entity.Parent.Node.Kind'Wide_Wide_Image);
         end if;

         An_Entity.Is_Null := True;
      else
         An_Entity.Reference := Get_Template_By_Name
           (An_Entity.Parent, An_Entity.Node.As_Template_Call.F_Name);

         if An_Entity.Reference = null then
            Error ("'" & Name & "' not found.");
         end if;
      end if;

      T_Entity_Type (An_Entity.all).Resolve_References;
   end Resolve_References;

   overriding
   procedure Resolve_References (An_Entity : access T_Command_Type) is
   begin
      if An_Entity.Defer then
         Compute_Closure (T_Entity (An_Entity), An_Entity.Deferred_Closure);
      end if;

      if An_Entity.Template_Section /= null then
         case An_Entity.Template_Section.Node.
           As_Template_Section.F_Actions.Kind
         is
            when Template_Traverse_Into =>
               An_Entity.Template_Section.A_Visit_Action := Into;

            when Template_Traverse_Over =>
               An_Entity.Template_Section.A_Visit_Action := Over;

            when others =>
               null;

         end case;
      end if;

      T_Entity_Type (An_Entity.all).Resolve_References;
   end Resolve_References;

   overriding
   procedure Resolve_References (An_Entity : access T_Module_Type) is
   begin
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

      T_Entity_Type (An_Entity.all).Resolve_References;
   end Resolve_References;

   overriding
   procedure Resolve_References (An_Entity : access T_Expr_Type) is
   begin
      if An_Entity.Kind = Template_Lambda_Expr then
         Compute_Closure (T_Entity (An_Entity.Lambda_Expr), An_Entity.Lambda_Closure);
      end if;

      --  TODO: There are a few cases where names can be resolved statically,
      --  e.g. static references in identifiers

      T_Entity_Type (An_Entity.all).Resolve_References;
   end Resolve_References;

   procedure Compute_Closure (Root : T_Entity; Closure : in out Text_Sets.Set) is
      Local_Symbols : Text_Sets.Set;

      procedure Not_Capture_Identifiers (Expr : T_Expr);

      procedure Capture_Identifiers (Expr : T_Expr);

      procedure Capture (Name : Text_Type) is
      begin
         if Local_Symbols.Contains (Name) then
            --  If the symbol is local to the lambda, then there's nothing to
            --  capture.
            return;
         else
            --  Otherwise, record this symbol to be captured.

            Closure.Include (Name);
         end if;
      end Capture;

      procedure Capture_Identifiers (Expr : T_Expr) is
      begin
         Push_Error_Location (Expr.Node);

         case Expr.Kind is
            when Template_Selector =>
               Capture_Identifiers (Expr.Selector_Left);
               Not_Capture_Identifiers (Expr.Selector_Right);

            when Template_Token_Identifier | Template_Identifier =>
               Capture (Expr.Node.Text);

            when others =>
               Not_Capture_Identifiers (Expr);
         end case;

         Pop_Error_Location;
      end Capture_Identifiers;

      procedure Not_Capture_Identifiers (Expr : T_Expr) is
      begin
         Push_Error_Location (Expr.Node);

         case Expr.Kind is
            when Template_Token_Identifier
               | Template_Identifier
               | Template_Literal
               | Template_Number
               | Template_At_Ref
               | Template_Reg_Expr_Anchor =>

               null;

            when Template_Match_Capture =>
               declare
                  Name : Text_Type := Expr.Node.As_Match_Capture.F_Captured.Text;
               begin
                  --  If the name isn't already identified as a local name,
                  --  identify it as such for the remainder of the analysis.
                  --  Otherwise, just pass through.

                  if not Local_Symbols.Contains (Name) then
                     Local_Symbols.Insert (Name);
                     Capture_Identifiers (Expr.Match_Capture_Expr);
                     Local_Symbols.Delete (Name);
                     return;
                  else
                     Capture_Identifiers (Expr.Match_Capture_Expr);
                  end if;
               end;
            when Template_Str =>
               --  Analyze_Replace_String ABI is expecting that capturing an
               --  expression pushes a value on the stack (it's going to get
               --  popped. So use Capture_Expression_And_Push_Dummy in order
               --  to avoid popping the top of the stack.

               for S of Expr.Str loop
                  case S.Kind is
                     when Expr_Kind =>
                        Capture_Identifiers (S.Expr);

                     when Group_Kind =>
                        Error ("capture of groups not yet implemented");

                     when others =>
                        null;
                  end case;
               end loop;

            when Template_Binary_Expr
               | Template_Unary_Expr
               | Template_Qualified_Match
               | Template_All_Expr
               | Template_Fold_Expr
               | Template_Lambda_Expr
               | Template_Call_Expr
               | Template_New_Expr
               | Template_Reg_Expr
               | Template_Reg_Expr_Quantifier
               | Template_Match_Expr =>

               for C of Expr.Children_Ordered loop
                  if C.all in T_Expr_Type'Class then
                     Capture_Identifiers (T_Expr (C));
                  elsif C.all in T_Arg_Type'Class then
                     Capture_Identifiers (T_Arg (C).Expr);
                  end if;
               end loop;

            when others =>
               Error
                 ("unhandled expression kind in capture identifers: "
                  & Expr.Kind'Wide_Wide_Image);
         end case;

         Pop_Error_Location;
      end Not_Capture_Identifiers;

      procedure Capture_Identifiers_In_Expressions (Entity : T_Entity) is
      begin
         --  TODO: handle var and their scope in the list of local symbols
         if Entity.all in T_Expr_Type'Class then
            Capture_Identifiers (T_Expr (Entity));
         else
            for C of Entity.Children_Ordered loop
               Capture_Identifiers_In_Expressions (C);
            end loop;
         end if;
      end Capture_Identifiers_In_Expressions;

   begin
      Capture_Identifiers_In_Expressions (Root);
   end Compute_Closure;

end Wrapping.Semantic.Structure;
