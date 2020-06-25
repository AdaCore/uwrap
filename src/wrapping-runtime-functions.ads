with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;

package Wrapping.Runtime.Functions is

   procedure Build_Lambda
     (Object : access W_Object_Type'Class;
      Params : Argument_List);

   procedure Normalize_Ada_Name
     (Object : access W_Object_Type'Class;
      Params : Argument_List);

   --  TODO: Reconcider these functions - should probably be function references
   --  instead, and using lambda when their result needs to be defered (as
   --  opposed to defering implicitely right now.

   type W_Call_To_Global_Type;
   type W_Call_To_Global is access all W_Call_To_Global_Type'Class;

   type W_Call_To_Lower_Type;
   type W_Call_To_Lower is access all W_Call_To_Lower_Type'Class;

   type W_Call_Unindent_Type;
   type W_Call_Unindent is access all W_Call_Unindent_Type'Class;

   type W_Call_To_Global_Type is abstract new W_Text_Expression_Type with record
      null;
   end record;

   procedure Analyze_Parameters (Call : in out W_Call_To_Global_Type; Params : Argument_List) is abstract;

   type W_Call_To_Lower_Type is new W_Call_To_Global_Type with record
      Param : W_Object;
   end record;

   overriding
   procedure Analyze_Parameters (Call : in out W_Call_To_Lower_Type; Params : Argument_List);

   overriding
   function To_String (Object : W_Call_To_Lower_Type) return Text_Type;

   type W_Call_Unindent_Type is new W_Call_To_Global_Type with record
      Param : W_Object;
   end record;

   overriding
   procedure Analyze_Parameters (Call : in out W_Call_Unindent_Type; Params : Argument_List);

   overriding
   function To_String (Object : W_Call_Unindent_Type) return Text_Type;

end Wrapping.Runtime.Functions;
