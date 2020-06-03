with Ada.Containers.Indefinite_Vectors;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;

package Wrapping.Semantic.Analysis is

   Root_Module : Semantic.Structure.Module := new Semantic.Structure.Module_Type;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   Files : String_Vectors.Vector;

   procedure Analyze;

   procedure Push_Error_Location (Node : Template_Node'Class);

end Wrapping.Semantic.Analysis;
