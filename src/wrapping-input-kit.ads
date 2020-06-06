with Libtestlang.Analysis; use Libtestlang.Analysis;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;
with Ada.Containers.Indefinite_Hashed_Maps;

package Wrapping.Input.Kit is

   type Kit_Language_Entity_Type;
   type Kit_Language_Entity is access all Kit_Language_Entity_Type'Class;

   package Kit_Language_Entity_Node_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Test_Node, Kit_Language_Entity, Hash, "=", "=");
   use Kit_Language_Entity_Node_Maps;

   type Kit_Language_Entity_Type is new Language_Entity_Type with record
      Node : Test_Node;
      Children_Computed : Boolean := False;
      Children_By_Node : Kit_Language_Entity_Node_Maps.Map;
   end record;

   overriding
   procedure Pre_Visit (An_Entity : access Kit_Language_Entity_Type);

   overriding
   function Push_Value
     (An_Entity : access Kit_Language_Entity_Type;
      Name      : Text_Type) return Boolean;

   overriding
   function Push_Match_Result
     (An_Entity : access Kit_Language_Entity_Type;
      Selector  : Runtime_Object;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   overriding
   function To_Text (Object : Kit_Language_Entity_Type) return Text_Type;

end Wrapping.Input.Kit;
