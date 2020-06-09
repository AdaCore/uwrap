with Libtemplatelang;
with Libtemplatelang.Analysis;

with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;

package Wrapping.Runtime.Analysis is

   Top_Frame : Data_Frame;

   Language_Class_Registry : Language_Entity_Class_Maps.Map;

   procedure Analyse (Language : Text_Type);

   procedure Analyse (Root_Entity : Language_Entity);

   -- TODO: Make a version of that function that pops and return the result.
   -- it should be used in many places
   procedure Evaluate_Expression
     (Node : Libtemplatelang.Analysis.Template_Node'Class);

   function Match (Pattern, Text : Text_Type) return Boolean;
   --  Match a pattern with a text, adding group and captured variables on the
   --  top frame

   procedure Push_Entity (An_Entity : access Language_Entity_Type'Class);

   procedure Push_Implicit_Self (An_Entity : access Language_Entity_Type'Class);

   procedure Push_Implicit_New (An_Entity : access Language_Entity_Type'Class);

   --  Push the temporary of the given name on the stack. If one needs to be
   --  created, counter will be used to append to the name and be incremented.
   procedure Push_Temporary_Name (Name : Text_Type; Counter : in out Integer);

   --  TODO: generalize the usage of this instead of manual deleting from the
   --  stack
   procedure Pop_Entity (Number : Positive := 1);

   function Pop_Entity return Runtime_Object;

end Wrapping.Runtime.Analysis;
