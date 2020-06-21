with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Text_IO;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;

package body Wrapping.Input.Kit is

   Global_Node_Registry : W_Kit_Node_Entity_Node_Maps.Map;

   procedure Call_Check_Expression
     (Object : access W_Object_Type'Class;
      Params : Libtemplatelang.Analysis.Argument_List)
   is
      Result : W_Object;
   begin
      if Params.Children_Count = 0 then
         Push_Match_True (Object);
      elsif Params.Children_Count = 1 then
         Push_Implicit_Self (Object);
         Evaluate_Expression (Params.Child (1).As_Argument.F_Value);
         Result := Pop_Object;
         Pop_Object;

         if Result /= Match_False then
            Push_Object (Object);
         else
            Push_Match_False;
         end if;
      elsif Params.Children_Count > 1 then
         Error ("matcher takes only 1 argument");
      end if;
   end Call_Check_Expression;

   procedure Pre_Visit (An_Entity : access W_Kit_Node_Type) is
      New_Entity : W_Node;
   begin
      if not An_Entity.Children_Computed then
         for C of An_Entity.Node.Children loop
            if not C.Is_Null then
               New_Entity := new W_Kit_Node_Type'(Node => C, others => <>);
               Add_Child (An_Entity, New_Entity);
               An_Entity.Children_By_Node.Insert (C, W_Kit_Node (New_Entity));
               Global_Node_Registry.Insert (C, W_Kit_Node (New_Entity));
            end if;
         end loop;

         An_Entity.Children_Computed := True;
      end if;
   end Pre_Visit;

   function Eval_Field (Node : Kit_Node; Name : Text_Type) return W_Object is
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
               return new W_Source_Node_Type'(A_Node => Eval_Field (Node, Field_Node));
            end if;
         end;
      end if;

      return null;
   end Eval_Field;

   function Get_Entity_For_Node (Node : Kit_Node) return W_Kit_Node is
      Parent : W_Kit_Node;
      New_Entity : W_Kit_Node;
   begin
      if Global_Node_Registry.Contains (Node) then
         return Global_Node_Registry.Element (Node);
      elsif not Node.Parent.Is_Null then
         Parent := Get_Entity_For_Node (Node.Parent);
         Parent.Pre_Visit;

         return
           W_Kit_Node_Type (Parent.all).
           Children_By_Node.Element (Node);
      else
         New_Entity := new W_Kit_Node_Type'(Node => Node, others => <>);
         Global_Node_Registry.Insert (Node, New_Entity);
         return New_Entity;
      end if;
   end Get_Entity_For_Node;

   function Eval_Property (Node : Kit_Node; Name : Text_Type) return W_Object is
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
               declare
                  Values : Value_Array (1 .. Property_Argument_Types (Property_Node)'Last);
               begin
                  for I in Values'Range loop
                     Values (I) := Property_Argument_Default_Value (Property_Node, I);
                  end loop;

                  Value := Eval_Property (Node, Property_Node, Values);
               end;

               if Kind (Value) = Text_Type_Value then
                  return new W_String_Type'
                    (Value => To_Unbounded_Text (As_Text_Type (Value)));
               elsif Kind (Value) = Node_Value then
                  if As_Node (Value).Is_Null then
                     return Match_False;
                  else
                     return W_Object (Get_Entity_For_Node (As_Node (Value)));
                  end if;
               else
                  Error ("unsupported property kind: " & Any_Value_Kind'Wide_Wide_Image (Kind (Value)));
               end if;
            end if;
         end;
      end if;

      return null;
   end Eval_Property;

   overriding
   function Push_Value
     (An_Entity : access W_Kit_Node_Type;
      Name      : Text_Type) return Boolean
   is
      Id : Any_Node_Type_Id;
   begin
      if W_Node_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      Id := Lookup_DSL_Name (To_String (Name));

      if Id /= No_Node_Type_Id and then Is_Derived_From (Id_For_Kind (An_Entity.Node.Kind), Id) then
         --  We are in something of the form
         --  An_Entity (Node_Type ());
         --  the type matched. Stack a function that will verify the sub
         --  expression.
         --  TODO: This also accepts An_Entity.Node_Type() which might be
         --  bizzare... Need to decide if this is OK.

         Push_Object
           (W_Object'
              (new W_Function_Type'
                   (Prefix => W_Object (An_Entity),
                    Call   => Call_Check_Expression'Unrestricted_Access)));
         return True;
      end if;

      declare
         Result : W_Object;
      begin
         Result := Eval_Field (An_Entity.Node, Name);

         if Result = null then
            Result := Eval_Property (An_Entity.Node, Name);
         end if;

         if Result /= null then
            if Result.all in W_Source_Node_Type'Class then
               Push_Object
                 (An_Entity.Children_By_Node.Element (W_Source_Node_Type (Result.all).A_Node));
            else
               Push_Object (Result);
            end if;

            return True;
         end if;
      end;

      return False;
   end Push_Value;

   overriding
   function To_String (Object : W_Kit_Node_Type) return Text_Type is
   begin
      return Object.Node.Text;
   end To_String;

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
      Root_Entity : W_Node;
   begin
      Root_Entity := W_Node (Get_Entity_For_Node (Unit.Root));

      Wrapping.Runtime.Analysis.Analyse (Root_Entity);
   end Analyze_Unit;

end Wrapping.Input.Kit;
