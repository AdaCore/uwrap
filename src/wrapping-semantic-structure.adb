with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Containers; use Ada.Containers;

with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Wrapping.Utils; use Wrapping.Utils;

package body Wrapping.Semantic.Structure is

   procedure Add_Child (Parent, Child : access Entity_Type'Class) is
   begin
      Child.Parent := Entity (Parent);

      if Parent.Children_Ordered.Length > 0 then
         Parent.Children_Ordered.Last_Element.Next := Entity (Child);
         Child.Prev := Parent.Children_Ordered.Last_Element;
      end if;

      Parent.Children_Ordered.Append (Entity (Child));
   end Add_Child;

   procedure Add_Child (Parent, Child : access Entity_Type'Class; Name_Node : Template_Node'Class) is
   begin
      Add_Child (Parent, Child);
      Parent.Children_Indexed.Insert (Name_Node.Text, Entity (Child));
   end Add_Child;

   procedure Add_Child (Parent, Child : access Entity_Type'Class; Name : Text_Type) is
   begin
      Add_Child (Parent, Child);
      Parent.Children_Indexed.Insert (Name, Entity (Child));
   end Add_Child;

   function Full_Name (An_Entity : Entity_Type) return Text_Type is
   begin
      if An_Entity.Parent = null then
         return "";
      else
         return An_Entity.Parent.Full_Name;
      end if;
   end Full_Name;

   function Find_Visible_Entity (An_Entity : Entity_Type'Class; Name : Text_Type) return Entity
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

   function Full_Name (An_Entity : Named_Entity_Type) return Text_Type is
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

   function Resolve_Module_By_Name (Name : Text_Type) return Module is
      Result : Entity;
      A_Namespace : Namespace;
      A_Suffix : Text_Type := Suffix (Name);
   begin
      A_Namespace := Get_Namespace_Prefix (Name);

      if A_Namespace.Children_Indexed.Contains (A_Suffix) then
         Result := A_Namespace.Children_Indexed.Element (A_Suffix);

         if Result.all in Module_Type then
            return Module (Result);
         end if;
      end if;

      return null;
   end Resolve_Module_By_Name;

   function Instance_Of (Child, Parent : Template) return Boolean is
   begin
      if Child = Parent then
         return True;
      elsif Child.Extends = null then
         return False;
      else
         return Instance_Of (Child.Extends, Parent);
      end if;
   end Instance_Of;

   function Has_Variable (A_Template : Template_Type; Name : Text_Type) return Boolean is
   begin
      if A_Template.Variables_Indexed.Contains (Name) then
         return True;
      elsif A_Template.Extends /= null then
         return Has_Variable (A_Template.Extends.all, Name);
      else
         return False;
      end if;
   end Has_Variable;

   function Get_Variable_For_Index (A_Template : Template_Type; Index : Positive) return Var
   is
      Result : Var;

      procedure Recursive_Search (A_Template : Template_Type'Class; Start_Offset : in out Integer) is
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

   function Get_Component (A_Template : Template_Type; Name : Text_Type) return Entity is
   begin
      if A_Template.Children_Indexed.Contains (Name) then
         return A_Template.Children_Indexed.Element (Name);
      elsif A_Template.Extends = null then
         return null;
      else
         return A_Template.Extends.Get_Component (Name);
      end if;
   end Get_Component;

   function Get_Namespace_Prefix (Full_Name : Text_Type; Create_If_Null : Boolean := False) return Namespace is
      First, Dot : Integer;
      Tentative : Entity;
      Current : Namespace := Wrapping.Semantic.Analysis.Root;
      New_Namespace : Namespace;
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

                  if Tentative.all not in Namespace_Type then
                     Error ("Expected namespace");
                  else
                     Current := Namespace (Tentative);
                  end if;
               elsif Create_If_Null then
                  New_Namespace := new Namespace_Type;
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

end Wrapping.Semantic.Structure;
