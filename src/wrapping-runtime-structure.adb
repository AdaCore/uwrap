with Ada.Containers; use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;
with Libtemplatelang.Common; use Libtemplatelang.Common;
with Wrapping.Semantic.Structure;
with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Wrapping.Runtime.Functions; use Wrapping.Runtime.Functions;
with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;

package body Wrapping.Runtime.Structure is

   Root_Language_Entities : W_Node_Maps.Map;

   function Get_Visible_Symbol (A_Frame: Data_Frame_Type; Name : Text_Type) return W_Object is
   begin
      if A_Frame.Symbols.Contains (Name) then
         return A_Frame.Symbols.Element (Name);
      elsif A_Frame.Parent_Frame /= null then
         return Get_Visible_Symbol (A_Frame.Parent_Frame.all, Name);
      else
         --  Return one of the global function.
         --  TODO: we probably want some mechanism to allow this to be extended
         if Name = "unindent" then
            return new W_Call_Unindent_Type;
         elsif Name = "to_lower" then
            return new W_Call_To_Lower_Type;
         else
            return null;
         end if;
      end if;
   end Get_Visible_Symbol;

   function Get_Module (A_Frame : Data_Frame_Type) return Semantic.Structure.Module is
      use Semantic.Structure;

      Scope : Semantic.Structure.Entity := A_Frame.Lexical_Scope;
   begin
      while Scope /= null and then Scope.all not in Module_Type'Class loop
         Scope := Scope.Parent;
      end loop;

      return Semantic.Structure.Module (Scope);
   end Get_Module;

   procedure Push_Match_True (An_Entity : access W_Object_Type'Class) is
   begin
      Push_Object (An_Entity);
   end Push_Match_True;

   procedure Push_Match_False is
   begin
      Push_Object (Match_False);
   end Push_Match_False;

   procedure Push_Call_Result
     (An_Entity : access W_Object_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) is
   begin
      Error ("non callable entity");
   end Push_Call_Result;

   Module_Registry : W_Object_Maps.Map;

   function Get_Object_For_Module
     (A_Module : Wrapping.Semantic.Structure.Module) return W_Object
   is
      Result : W_Template_Instance;
      Name : Text_Type := A_Module.Full_Name;
   begin
      if Module_Registry.Contains (Name) then
         return Module_Registry.Element (Name);
      else
         Result := new W_Template_Instance_Type;

         for V of A_Module.Variables_Ordered loop
            case V.Kind is
               when Map_Kind =>
                  Result.Symbols.Insert
                    (V.Name_Node.Text, new W_Reference_Type'
                       (Value => new W_Map_Type, others => <>));

               when Text_Kind =>
                  --  Text is currently modelled as a reference to a text
                  --  container.
                  Result.Symbols.Insert
                    (V.Name_Node.Text, new W_Reference_Type'
                       (Value => new W_Vector_Type, others => <>));

               when others =>
                  Error ("global variable type not yet supported");

            end case;
         end loop;

         Module_Registry.Insert (Name, W_Object (Result));
         return W_Object (Result);
      end if;
   end Get_Object_For_Module;

end Wrapping.Runtime.Structure;
