with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;

package Wrapping.Runtime.Functions is

   procedure Call_Normalize_Ada_Name
     (Object : access W_Object_Type'Class;
      Params : Argument_List);

   procedure Call_Replace_Text
     (Object : access W_Object_Type'Class;
      Params : Argument_List);

   procedure Call_To_Lower
     (Object : access W_Object_Type'Class;
      Params : Argument_List);

   procedure Call_Unindent
     (Object : access W_Object_Type'Class;
      Params : Argument_List);

end Wrapping.Runtime.Functions;
