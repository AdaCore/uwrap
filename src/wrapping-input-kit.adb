with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Text_IO;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;

package body Wrapping.Input.Kit is

   Global_Node_Registry : Kit_Language_Entity_Node_Maps.Map;

   procedure Pre_Visit (An_Entity : access Kit_Language_Entity_Type) is
      New_Entity : Language_Entity;
   begin
      if not An_Entity.Children_Computed then
         for C of An_Entity.Node.Children loop
            if not C.Is_Null then
               New_Entity := new Kit_Language_Entity_Type'(Node => C, others => <>);
               Add_Child (An_Entity, New_Entity);
               An_Entity.Children_By_Node.Insert (C, Kit_Language_Entity (New_Entity));
               Global_Node_Registry.Insert (C, Kit_Language_Entity (New_Entity));
            end if;
         end loop;

         An_Entity.Children_Computed := True;
      end if;
   end Pre_Visit;

   function Eval_Field (Node : Kit_Node; Name : Text_Type) return Runtime_Object is
      Field_Node : Any_Node_Data_Reference;
   begin
      if Name'Length > 2
        and then Name (Name'First .. Name'First + 1) = "f_"
      then
         declare
            F_Name : Text_Type := To_Lower (Name (Name'First + 2 .. Name'Last));
         begin
            Field_Node := Lookup_Node_Data (Id_For_Kind (Node.Kind), To_String (F_Name));

            if Field_Node /= None then
               return new Runtime_Node_Type'(A_Node => Eval_Field (Node, Field_Node));
            end if;
         end;
      end if;

      return null;
   end Eval_Field;

   function Get_Entity_For_Node (Node : Kit_Node) return Kit_Language_Entity is
      Parent : Kit_Language_Entity;
      New_Entity : Kit_Language_Entity;
   begin
      if Global_Node_Registry.Contains (Node) then
         return Global_Node_Registry.Element (Node);
      elsif not Node.Parent.Is_Null then
         Parent := Get_Entity_For_Node (Node.Parent);
         Parent.Pre_Visit;

         return
           Kit_Language_Entity_Type (Parent.all).
           Children_By_Node.Element (Node);
      else
         New_Entity := new Kit_Language_Entity_Type'(Node => Node, others => <>);
         Global_Node_Registry.Insert (Node, New_Entity);
         return New_Entity;
      end if;
   end Get_Entity_For_Node;


   function Eval_Property (Node : Kit_Node; Name : Text_Type) return Runtime_Object is
      Property_Node : Any_Node_Data_Reference;
   begin
      if Name'Length > 2
        and then Name (Name'First .. Name'First + 1) = "p_"
      then
         declare
            P_Name : Text_Type := To_Lower (Name (Name'First + 2 .. Name'Last));
            Value : Value_Type;
         begin
            Property_Node := Lookup_Node_Data (Id_For_Kind (Node.Kind), To_String (P_Name));

            if Property_Node /= None then
               Value := Eval_Property (Node, Property_Node, (1 .. 0 => <>));

               if Kind (Value) = Text_Type_Value then
                  return new Runtime_Text_Type'
                    (Value => To_Unbounded_Text (As_Text_Type (Value)));
               elsif Kind (Value) = Node_Value then
                  return new Runtime_Language_Entity_Type'
                    (Value => Language_Entity
                       (Get_Entity_For_Node (As_Node (Value))), others => <>);
               else
                  Error ("unsupported property kind: " & Any_Value_Kind'Wide_Wide_Image (Kind (Value)));
               end if;
            end if;
         end;
      end if;

      return null;
   end Eval_Property;


   function Push_Value
     (An_Entity : access Kit_Language_Entity_Type;
      Name      : Text_Type) return Boolean
   is
   begin
      if Language_Entity_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      declare
         Result : Runtime_Object;
      begin
         Result := Eval_Field (An_Entity.Node, Name);

         if Result = null then
            Result := Eval_Property (An_Entity.Node, Name);
         end if;

         if Result /= null then
            if Result.all in Runtime_Node_Type then
               Push_Entity
                 (An_Entity.Children_By_Node.Element (Runtime_Node_Type (Result.all).A_Node));
            else
               Top_Frame.Data_Stack.Append (Result);
            end if;

            return True;
         end if;
      end;

      return False;
   end Push_Value;

   overriding
   function Push_Match_Result
     (An_Entity : access Kit_Language_Entity_Type;
      Selector  : Runtime_Object;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean
   is
      Result : Runtime_Object;
      Name : Text_Type := Selector.To_Text;
      Matched : Boolean;
      Id : Any_Node_Type_Id;
   begin
      if Language_Entity_Type (An_Entity.all).Push_Match_Result (Selector, Params) then
         return True;
      end if;

      if Selector.all in Runtime_Field_Reference_Type'Class then
         Id := Lookup_DSL_Name (To_String (Name));

         if Id /= No_Node_Type_Id and then Is_Derived_From (Id_For_Kind (An_Entity.Node.Kind), Id) then
            if Params.Children_Count = 0 then
               Push_Match_True (An_Entity);
            elsif Params.Children_Count = 1 then
               -- fields are browsing functions similar to prev, next, etc...
               -- they push the node so that it can be captured.
               Push_Implicit_Self (An_Entity);

               Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

               Result := Top_Frame.Data_Stack.Last_Element;
               Top_Frame.Data_Stack.Delete_Last (2); -- Delete result and self

               Matched := Result /= Match_False;

               if Matched then
                  Push_Match_True (An_Entity);
               else
                  Push_Match_False;
               end if;
            else
               Error ("no more than one parameter allowed for entity matching");
            end if;

            return True;
         else
            declare
               Result : Runtime_Object;
               Node : Kit_Node;
               Field_Entity : Kit_Language_Entity;
            begin
               Result := Eval_Field (An_Entity.Node, Name);

               if Result = null then
                  Result := Eval_Property (An_Entity.Node, Name);
               end if;

               if Result /= null then
                  Node := Runtime_Node_Type (Result.all).A_Node;
                  Field_Entity := An_Entity.Children_By_Node.Element (Node);

                  if Params.Children_Count = 0 then
                     Push_Match_True (Field_Entity);
                  elsif Params.Children_Count /= 1 then
                     Error ("no more than one parameter allowed for field matching");
                  elsif Result.all in Runtime_Node_Type then
                     Node := Runtime_Node_Type (Result.all).A_Node;

                     -- fields are browsing functions similar to prev, next, etc...
                     -- they push the node so that it can be captured.
                     Push_Implicit_Self (An_Entity.Children_By_Node.Element (Node));

                     Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

                     Result := Top_Frame.Data_Stack.Last_Element;
                     Top_Frame.Data_Stack.Delete_Last (2); -- Delete result and self

                     Matched := Result /= Match_False;

                     if Matched then
                        Push_Match_True (Field_Entity);
                     else
                        Push_Match_False;
                     end if;
                  elsif Result.all in Runtime_Text_Type then
                     Error ("matching text property not yet implemented");
                  end if;

                  return True;
               end if;
            end;
         end if;
      elsif Selector.all in Runtime_Text_Expression_Type'Class then
         if Match (Name, An_Entity.Node.Text) then
            Push_Match_True (Selector);
         else
            Push_Match_False;
         end if;

         return True;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   function To_Text (Object : Kit_Language_Entity_Type) return Text_Type is
   begin
      return Object.Node.Text;
   end To_Text;

   procedure Analyze_File (File : String) is
      Unit : Analysis_Unit;
      Context : Analysis_Context := Create_Context;

   begin
      Unit := Get_From_File (Context, File);

      if Has_Diagnostics (Unit) then
         for D of Diagnostics (Unit) loop
            Ada.Text_IO.Put_Line (File & ":" & To_Pretty_String (D));
         end loop;
      end if;

      Analyze_Unit (Unit);
   end Analyze_File;

   procedure Analyze_Unit (Unit : Analysis_Unit) is
      Root_Entity : Language_Entity;
   begin
      Root_Entity := Language_Entity (Get_Entity_For_Node (Unit.Root));

      Wrapping.Runtime.Analysis.Analyse (Root_Entity);
   end Analyze_Unit;

end Wrapping.Input.Kit;
