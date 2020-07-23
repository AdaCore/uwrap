with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;

package Wrapping.Runtime.Functions is

   procedure Call_Normalize_Ada_Name
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector);

   procedure Call_Replace_Text
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector);

   procedure Call_To_Lower
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector);

   procedure Call_Reindent
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector);

end Wrapping.Runtime.Functions;
