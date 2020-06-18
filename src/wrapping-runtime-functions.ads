with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;

package Wrapping.Runtime.Functions is

   type Runtime_Call_To_Global_Type;
   type Runtime_Call_To_Global is access all Runtime_Call_To_Global_Type'Class;

   type Runtime_Call_To_Lower_Type;
   type Runtime_Call_To_Lower is access all Runtime_Call_To_Lower_Type'Class;

   type Runtime_Call_Unindent_Type;
   type Runtime_Call_Unindent is access all Runtime_Call_Unindent_Type'Class;

   type Runtime_Call_To_Global_Type is abstract new Runtime_Text_Expression_Type with record
      null;
   end record;

   procedure Analyze_Parameters (Call : in out Runtime_Call_To_Global_Type; Params : Argument_List) is abstract;

   type Runtime_Call_To_Lower_Type is new Runtime_Call_To_Global_Type with record
      Param : Runtime_Object;
   end record;

   overriding
   procedure Analyze_Parameters (Call : in out Runtime_Call_To_Lower_Type; Params : Argument_List);

   overriding
   function To_Text (Object : Runtime_Call_To_Lower_Type) return Text_Type;

   type Runtime_Call_Unindent_Type is new Runtime_Call_To_Global_Type with record
      Param : Runtime_Object;
   end record;

   overriding
   procedure Analyze_Parameters (Call : in out Runtime_Call_Unindent_Type; Params : Argument_List);

   overriding
   function To_Text (Object : Runtime_Call_Unindent_Type) return Text_Type;

end Wrapping.Runtime.Functions;
