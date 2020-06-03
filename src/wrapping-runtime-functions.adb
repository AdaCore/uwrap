with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Wrapping.Utils; use Wrapping.Utils;

package body Wrapping.Runtime.Functions is

   overriding
   procedure Analyze_Parameters (Call : in out Runtime_Call_To_Lower_Type; Params : Argument_List) is
   begin
      if Params.Children_Count /= 1 then
         Error ("to_lower needs to be called with one parameter");
      end if;

      Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

      Call.Param := Top_Frame.Data_Stack.Last_Element;
      Top_Frame.Data_Stack.Delete_Last;
   end Analyze_Parameters;

   -------------
   -- To_Text --
   -------------

   overriding function To_Text
     (Object : Runtime_Call_To_Lower_Type) return Text_Type
   is
   begin
      return To_Lower (Object.Param.To_Text);
   end To_Text;

   ------------------------
   -- To_Text_Expression --
   ------------------------

   overriding function To_Text_Expression
     (Object : access Runtime_Call_To_Lower_Type)
      return Runtime_Text_Expression
   is
   begin
      return Runtime_Text_Expression (Object);
   end To_Text_Expression;


   overriding
   procedure Analyze_Parameters (Call : in out Runtime_Call_Unindent_Type; Params : Argument_List) is
   begin
      if Params.Children_Count /= 1 then
         Error ("unindent needs to be called with one parameter");
      end if;

      Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

      Call.Param := Top_Frame.Data_Stack.Last_Element;
      Top_Frame.Data_Stack.Delete_Last;
   end Analyze_Parameters;


   -------------
   -- To_Text --
   -------------

   overriding function To_Text
     (Object : Runtime_Call_Unindent_Type) return Text_Type
   is
   begin
      return Unident (Object.Param.To_Text);
   end To_Text;

   ------------------------
   -- To_Text_Expression --
   ------------------------

   overriding function To_Text_Expression
     (Object : access Runtime_Call_Unindent_Type)
      return Runtime_Text_Expression
   is
   begin
      return Runtime_Text_Expression (Object);
   end To_Text_Expression;

end Wrapping.Runtime.Functions;
