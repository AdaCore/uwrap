with Ada.Containers.Hashed_Maps;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with Libadalang.Common; use Libadalang.Common;
with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;

package body Wrapping.Input.Ada is

   package Ada_Names_Maps is new Standard.Ada.Containers.Hashed_Maps
     (Ada_Node, Ada_Language_Entity, Hash, "=");
   use Ada_Names_Maps;

   Ada_Names : Ada_Names_Maps.Map;
   Ada_Language_Entity_Stack : Ada_Language_Entity_Vectors.Vector;

   function Visit_Unit (Node : Ada_Node'Class) return Visit_Status;
   function Visit_Package (Node : Ada_Node'Class) return Visit_Status;
   function Visit_Subprogram (Node : Ada_Node'Class) return Visit_Status;
   function Visit_Parameter (Node : Ada_Node'Class) return Visit_Status;
   function Visit_Return_Type (Node : Ada_Node'Class) return Visit_Status;
   function Visit_Object (Node : Ada_Node'Class) return Visit_Status;
   function Visit_Type (Node : Ada_Node'Class) return Visit_Status;
   function Visit_Name (Node : Ada_Node'Class) return Visit_Status;

   overriding
   function Push_Value
     (An_Entity : access Ada_Language_Entity_Type;
      Name      : Text_Type) return Boolean is
   begin
      if Language_Entity_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      return False;
   end Push_Value;

   function Push_Call_Result
     (An_Entity : access Ada_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean
   is
   begin
      if Language_Entity_Type (An_Entity.all).Push_Call_Result (Name, Params) then
         return True;
      end if;

      return False;
   end Push_Call_Result;

   overriding
   function Push_Match_Result
     (An_Entity : access Ada_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean
   is
   begin
      if Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "ada" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         else
            Error ("no arguments allowed for Ada predicate");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   function To_Text (An_Entity : Ada_Language_Entity_Type) return Text_Type is
   begin
      if An_Entity.Node.Is_Null then
         return "null node";
      else
         return Full_Sloc_Image (An_Entity.Node);
      end if;
   end To_Text;

   overriding
   function Push_Match_Result
     (An_Entity : access Unit_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "unit" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Error ("unit takes 0 arguments");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   function Push_Match_Result
     (An_Entity : access Name_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean
   is
      Matched : Boolean;
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "name" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

            Matched :=
              Match
                (Top_Frame.Data_Stack.Last_Element.To_Text,
                 An_Entity.Node.Text);

            Top_Frame.Data_Stack.Delete_Last;

            if Matched then
               Push_Match_True (An_Entity);
            else
               Push_Match_False;
            end if;
         else
            Error ("name only takes 1 argument");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   function Push_Match_Result
     (An_Entity : access Package_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "package" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Error ("package takes 0 arguments");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   function Push_Match_Result
     (An_Entity : access With_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "with" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Error ("with takes 0 arguments");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   function Push_Match_Result
     (An_Entity : access Use_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "use" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Error ("use takes 0 arguments");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;


   overriding
   function Push_Match_Result
     (An_Entity : access Subprogram_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "subprogram" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Error ("subprogram takes 0 arguments");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   function Push_Match_Result
     (An_Entity : access Parameter_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "parameter" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Error ("parameter takes 0 arguments");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   function Push_Value
     (An_Entity : access Parameter_Language_Entity_Type;
      Name      : Text_Type) return Boolean
   is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      if Name = "designated_type" then
         Push_Entity (An_Entity.Type_Name.Parent);

         return True;
      end if;

      return False;
   end Push_Value;

   overriding
   function Push_Match_Result
     (An_Entity : access Return_Type_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "return" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Error ("return takes 0 arguments");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   function Push_Value
     (An_Entity : access Return_Type_Language_Entity_Type;
      Name      : Text_Type) return Boolean
   is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      if Name = "designated_type" then
         Push_Entity (An_Entity.Type_Name.Parent);

         return True;
      end if;

      return False;
   end Push_Value;

   overriding
   function Push_Match_Result
     (An_Entity : access Type_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "type" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Error ("type takes 0 arguments");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   function Push_Match_Result
     (An_Entity : access Object_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Match_Result (Name, Params) then
         return True;
      end if;

      if Name = "object" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Error ("object takes 0 arguments");
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   function Push_Value
     (An_Entity : access Object_Language_Entity_Type;
      Name      : Text_Type) return Boolean is
   begin
      if Ada_Language_Entity_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      if Name = "designated_type" then
         Push_Entity (An_Entity.Type_Name.Parent);

         return True;
      end if;

      return False;
   end Push_Value;

   procedure Register_Globals is
   begin
      null;
   end Register_Globals;

   procedure Populate_Language_Entities (Root : Ada_Node) is
      Current_Entity_Access : Ada_Language_Entity := new Unit_Language_Entity_Type;
   begin
      Ada_Language_Entity_Stack.Append (Current_Entity_Access);
      Get_Root_Language_Entity ("ada").Add_Child (Current_Entity_Access);
      Root.Traverse (Visit_Unit'Access);
      Ada_Language_Entity_Stack.Delete_Last;
   end Populate_Language_Entities;

   function Get_Name_Language_Entity (Node : Ada_Node'Class) return Ada_Language_Entity is
      New_Entity : Ada_Language_Entity;
   begin
      if Ada_Names.Contains (Ada_Node (Node)) then
         New_Entity :=  Ada_Names.Element (Ada_Node (Node));
      else
         New_Entity := new Name_Language_Entity_Type'
           (Node => Ada_Node (Node), others => <>);
         Ada_Names.Insert (Ada_Node (Node), New_Entity);
      end if;

      return New_Entity;
   end Get_Name_Language_Entity;

   procedure Add_Child_And_Stack (An_Entity : Ada_Language_Entity) is
      Parent : Ada_Language_Entity := Ada_Language_Entity_Stack.Last_Element;
   begin
      Add_Child (Parent, An_Entity);

      Ada_Language_Entity_Stack.Append (An_Entity);
   end Add_Child_And_Stack;

   function Visit_Unit (Node : Ada_Node'Class) return Visit_Status is
      New_Entity : Ada_Language_Entity;
   begin
      case Node.Kind  is
         when Ada_With_Clause =>
            New_Entity := new With_Language_Entity_Type'(Node => Ada_Node (Node), others => <>);
            Add_Child_And_Stack (New_Entity);
            Node.Traverse (Visit_Name'Access);
            Ada_Language_Entity_Stack.Delete_Last;

            return Over;

         when Ada_Use_Clause =>
            New_Entity := new Use_Language_Entity_Type'(Node => Ada_Node (Node), others => <>);
            Add_Child_And_Stack (New_Entity);
            Node.Traverse (Visit_Name'Access);
            Ada_Language_Entity_Stack.Delete_Last;

            return Over;

         when Ada_Package_Decl =>
            New_Entity := new Package_Language_Entity_Type'(Node => Ada_Node (Node), others => <>);
            Add_Child_And_Stack (New_Entity);
            Node.Traverse (Visit_Name'Access);

            for C of Node.Children loop
               C.Traverse (Visit_Package'Access);
            end loop;

            Ada_Language_Entity_Stack.Delete_Last;

            return Over;

         when Ada_Package_Body =>
            -- Ignore, we don't wrap package bodies
            return Over;

         when others =>
            return Into;

      end case;
   end Visit_Unit;

   function Visit_Package (Node : Ada_Node'Class) return Visit_Status is
      New_Entity : Ada_Language_Entity;
   begin
      case Node.Kind is
         when Ada_Subp_Decl =>
            New_Entity := new Subprogram_Language_Entity_Type'
              (Node => Ada_Node (Node), others => <>);
            Add_Child_And_Stack (New_Entity);
            Node.Traverse (Visit_Name'Access);
            Node.Traverse (Visit_Subprogram'Access);
            Ada_Language_Entity_Stack.Delete_Last;

            return Over;

         when Ada_Object_Decl | Ada_Number_Decl =>
            New_Entity := new Object_Language_Entity_Type'
              (Node => Ada_Node (Node), others => <>);
            Add_Child_And_Stack (New_Entity);
            Node.Traverse (Visit_Name'Access);
            Node.Traverse (Visit_Object'Access);
            Ada_Language_Entity_Stack.Delete_Last;

            return Over;

         when Ada_Type_Decl =>
            New_Entity := new Type_Language_Entity_Type'
              (Node => Ada_Node (Node), others => <>);
            Add_Child_And_Stack (New_Entity);
            Node.Traverse (Visit_Name'Access);
            Node.Traverse (Visit_Type'Access);
            Ada_Language_Entity_Stack.Delete_Last;

            return Over;

         when Ada_Package_Decl =>
            New_Entity := new Package_Language_Entity_Type'
              (Node => Ada_Node (Node), others => <>);
            Add_Child_And_Stack (New_Entity);
            Node.Traverse (Visit_Name'Access);

            for C of Node.Children loop
               C.Traverse (Visit_Package'Access);
            end loop;

            Ada_Language_Entity_Stack.Delete_Last;

            return Over;

         when others =>
            return Into;

      end case;
   end Visit_Package;

   function Visit_Subprogram (Node : Ada_Node'Class) return Visit_Status is
      New_Entity : Ada_Language_Entity;
   begin
      case Node.Kind is
         when Ada_Param_Spec =>
            for Id of Node.As_Param_Spec.F_Ids loop
               New_Entity := new Parameter_Language_Entity_Type'
                 (Node => Ada_Node (Node), others => <>);
               Add_Child_And_Stack (New_Entity);
               Id.Traverse (Visit_Name'Access);
               Node.Traverse (Visit_Parameter'Access);
               Ada_Language_Entity_Stack.Delete_Last;
            end loop;

            return Over;

         when Ada_Subtype_Indication =>
            New_Entity := new Return_Type_Language_Entity_Type'
              (Node => Ada_Node (Node), others => <>);
            Add_Child_And_Stack (New_Entity);
            Node.Traverse (Visit_Return_Type'Access);
            Ada_Language_Entity_Stack.Delete_Last;

            return Over;

         when others =>
            return Into;
      end case;
   end Visit_Subprogram;

   function Visit_Parameter (Node : Ada_Node'Class) return Visit_Status is
      Current_Entity : Parameter_Language_Entity_Type renames
        Parameter_Language_Entity_Type (Ada_Language_Entity_Stack.Last_Element.all);
   begin
      case Node.Kind is
         when Ada_Subtype_Indication =>
            Current_Entity.Type_Name := Get_Name_Language_Entity
              (Node.As_Subtype_Indication.F_Name.P_Referenced_Defining_Name);

            return Over;

         when others =>
            return Into;
      end case;
   end Visit_Parameter;

   function Visit_Return_Type (Node : Ada_Node'Class) return Visit_Status is
      Current_Entity : Return_Type_Language_Entity_Type renames
        Return_Type_Language_Entity_Type (Ada_Language_Entity_Stack.Last_Element.all);
   begin
      case Node.Kind is
         when Ada_Subtype_Indication =>
            Current_Entity.Type_Name := Get_Name_Language_Entity
              (Node.As_Subtype_Indication.F_Name.P_Referenced_Defining_Name);

            return Over;

         when others =>
            return Into;
      end case;
   end Visit_Return_Type;

   function Visit_Object (Node : Ada_Node'Class) return Visit_Status is
      Current_Entity : Object_Language_Entity_Type renames
        Object_Language_Entity_Type (Ada_Language_Entity_Stack.Last_Element.all);
   begin
      case Node.Kind is
         when Ada_Subtype_Indication =>
            Current_Entity.Type_Name := Get_Name_Language_Entity
              (Node.As_Subtype_Indication.F_Name.P_Referenced_Defining_Name);

            return Over;

         when others =>
            return Into;
      end case;
   end Visit_Object;

   function Visit_Type (Node : Ada_Node'Class) return Visit_Status is
   begin
      return Over;
   end Visit_Type;

   function Visit_Name (Node : Ada_Node'Class) return Visit_Status is
      New_Entity : Ada_Language_Entity;
   begin
      case Node.Kind is
         when Ada_Defining_Name =>
            declare
               Name : Text_Type := Node.Text;
               Stack_Top : Ada_Language_Entity :=
                 Ada_Language_Entity_Stack.Last_Element;
            begin
               New_Entity := Get_Name_Language_Entity (Ada_Node (Node));
               Stack_Top.Name := New_Entity;

               if not Stack_Top.Parent.Children_Indexed.Contains (Name) then
                  Stack_Top.Parent.Children_Indexed.Insert (Name, Language_Entity (Stack_Top));
               else
                  declare
                     Copy_Id : Integer := 1;
                  begin
                     loop
                        declare
                           Copy_Name : Text_Type :=
                             Name & "<" & Trim (Copy_Id'Wide_Wide_Image, Left) & ">";
                        begin
                           if not Stack_Top.Parent.Children_Indexed.Contains (Copy_Name) then
                              Stack_Top.Parent.Children_Indexed.Insert
                                (Copy_Name, Language_Entity (Stack_Top));

                              exit;
                           end if;

                           Copy_Id := Copy_Id + 1;
                        end;
                     end loop;
                  end;
               end if;

               Add_Child_And_Stack (New_Entity);

               Ada_Language_Entity_Stack.Delete_Last;
            end;

            return Stop;

         when others =>
            return Into;
      end case;
   end Visit_Name;

end Wrapping.Input.Ada;
