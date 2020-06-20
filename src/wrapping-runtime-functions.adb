with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Wrapping.Utils; use Wrapping.Utils;

package body Wrapping.Runtime.Functions is

   overriding
   procedure Analyze_Parameters (Call : in out W_Call_To_Lower_Type; Params : Argument_List) is
   begin
      if Params.Children_Count /= 1 then
         Error ("to_lower needs to be called with one parameter");
      end if;

      Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

      Call.Param := Pop_Object;
   end Analyze_Parameters;

   -------------
   -- To_Text --
   -------------

   overriding function To_String
     (Object : W_Call_To_Lower_Type) return Text_Type
   is
   begin
      return To_Lower (Object.Param.To_String);
   end To_String;


   overriding
   procedure Analyze_Parameters (Call : in out W_Call_Unindent_Type; Params : Argument_List) is
   begin
      if Params.Children_Count /= 1 then
         Error ("unindent needs to be called with one parameter");
      end if;

      Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

      Call.Param := Pop_Object;
   end Analyze_Parameters;


   -------------
   -- To_Text --
   -------------

   overriding function To_String
     (Object : W_Call_Unindent_Type) return Text_Type
   is
   begin
      return Unident (Object.Param.To_String);
   end To_String;

end Wrapping.Runtime.Functions;
