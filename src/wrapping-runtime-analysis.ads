with Libtemplatelang;
with Libtemplatelang.Analysis;

with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;

package Wrapping.Runtime.Analysis is

   Top_Frame : Data_Frame;

   Language_Class_Registry : Language_Entity_Class_Maps.Map;

   procedure Analyse (Language : Text_Type);

   procedure Analyse (Root_Entity : Language_Entity);

   procedure Evaluate_Expression
     (Node : Libtemplatelang.Analysis.Template_Node'Class);

   function Match (Pattern, Text : Text_Type) return Boolean;
   --  Match a pattern with a text, adding group and captured variables on the
   --  top frame

end Wrapping.Runtime.Analysis;
