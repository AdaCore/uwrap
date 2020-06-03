with Libtestlang.Analysis; use Libtestlang.Analysis;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

package Wrapping.Input.Kit is

   type Kit_Language_Entity_Type is new Language_Entity_Type with record
      Node : Test_Node;
      Children_Computed : Boolean := False;
   end record;

   function Traverse
     (An_Entity : access Kit_Language_Entity_Type;
      A_Mode    : Browse_Mode;
      Include_Self : Boolean;
      Visitor   : access function (E : access Language_Entity_Type'Class) return Visit_Action)
      return Visit_Action;

   overriding
   function Push_Value
     (An_Entity : access Kit_Language_Entity_Type;
      Name      : Text_Type) return Boolean;

   overriding
   function Push_Match_Result
     (An_Entity : access Kit_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

end Wrapping.Input.Kit;
