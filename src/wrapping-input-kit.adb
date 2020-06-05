with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with Libtestlang.Introspection; use Libtestlang.Introspection;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Libtestlang.Common; use Libtestlang.Common;

package body Wrapping.Input.Kit is

   procedure Pre_Visit (An_Entity : access Kit_Language_Entity_Type) is
      New_Entity : Language_Entity;
   begin
       if not An_Entity.Children_Computed then
            for C of An_Entity.Node.Children loop
               New_Entity := new Kit_Language_Entity_Type'(Node => C, others => <>);
               Add_Child (An_Entity, New_Entity);
            end loop;

            An_Entity.Children_Computed := True;
      end if;
   end Pre_Visit;

   function Get_Field (Node : Test_Node; Name : Text_Type) return Test_Node is
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
               return Eval_Field (Node, Field_Node);
            end if;
         end;
      end if;

      return No_Test_Node;
   end Get_Field;


   function Push_Value
     (An_Entity : access Kit_Language_Entity_Type;
      Name      : Text_Type) return Boolean
   is
   begin
      if Language_Entity_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      declare
         Node : Test_Node := Get_Field (An_Entity.Node, Name);
      begin
         if not Node.Is_Null then
            Top_Frame.Data_Stack.Append
              (new Runtime_Text_Type'
                 (Value => To_Unbounded_Text (Node.Text)));

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
   begin
      if Language_Entity_Type (An_Entity.all).Push_Match_Result (Selector, Params) then
         return True;
      end if;

      if Name = "test_node" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         else
            Error ("no arguments allowed");
         end if;

         return True;
      elsif To_Lower (To_Wide_Wide_String (An_Entity.Node.Kind_Name)) = To_Lower (Name) then
         Push_Match_True (An_Entity);
         return True;
      else
         declare
            Node : Test_Node := Get_Field (An_Entity.Node, Name);
            Matched : Boolean;
         begin
            if not Node.Is_Null then
               if Params.Children_Count = 0 then
                  Push_Match_True (An_Entity);
               elsif Params.Children_Count = 1 then
                  Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

                  Result := Top_Frame.Data_Stack.Last_Element;
                  Top_Frame.Data_Stack.Delete_Last;

                  Matched :=
                    Match
                      (Result.To_Text,
                       Node.Text);

                  if Matched then
                     Push_Match_True (An_Entity);
                  else
                     Push_Match_False;
                  end if;
               else
                  Error ("no more than one parameter allowed for field matching");
               end if;

               return True;
            end if;
         end;
      end if;

      return False;
   end Push_Match_Result;

end Wrapping.Input.Kit;
