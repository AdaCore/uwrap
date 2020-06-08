with Libtestlang.Analysis; use Libtestlang.Analysis;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Ada.Containers.Indefinite_Hashed_Maps;

generic
   type Kit_Node is tagged private;
   type Kit_Node_Array is array (Positive range <>) of Kit_Node;
   type Any_Node_Data_Reference is (<>);
   type Any_Node_Type_Id is (<>);
   type Kit_Node_Kind_Type is (<>);
   type Analysis_Unit is tagged private;
   type Analysis_Context is tagged private;
   type Grammar_Rule is (<>);
   type Unit_Provider_Reference is private;
   type Any_Value_Kind is (<>);
   type Value_Type is private;
   type Value_Array is array (Positive range <>) of Value_Type;

   None : Any_Node_Data_Reference;
   No_Kit_Node : Kit_Node;
   Default_Grammar_Rule : Grammar_Rule;
   Default_Charset : String;
   No_Unit_Provider_Reference : Unit_Provider_Reference;
   No_Node_Type_Id : Any_Node_Type_Id;

   Text_Type_Value : Any_Value_Kind;

   with function Children (Node : Kit_Node'Class) return Kit_Node_Array is <>;
   with function Hash (Node : Kit_Node) return Ada.Containers.Hash_Type is <>;
   with function Lookup_Node_Data
     (Id : Any_Node_Type_Id; Name : String) return Any_Node_Data_Reference is <>;
   with function Id_For_Kind (Kind : Kit_Node_Kind_Type) return Any_Node_Type_Id is <>;
   with function Eval_Field
     (Node : Kit_Node'Class; Field : Any_Node_Data_Reference) return Kit_Node is <>;
   with function Kind (Node : Kit_Node'Class) return Kit_Node_Kind_Type is <>;
   with function Is_Null (Node : Kit_Node'Class) return Boolean is <>;
   with function Kind_Name (Node : Kit_Node'Class) return String is <>;
   with function Text (Node : Kit_Node'Class) return Text_Type is <>;
   with function Create_Context
     (Charset       : String                  := Default_Charset;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      With_Trivia   : Boolean := True; Tab_Stop : Positive := 8)
      return Analysis_Context is <>;
   with function Get_From_File
     (Context : Analysis_Context'Class; Filename : String;
      Charset : String       := ""; Reparse : Boolean := False;
      Rule    : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is <>;
   with function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean is <>;
   with function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array is <>;
   with function Root (Unit : Analysis_Unit'Class) return Kit_Node is <>;
   with function Lookup_DSL_Name (Name : String) return Any_Node_Type_Id is <>;
   with function Is_Derived_From (Id, Parent : Any_Node_Type_Id) return Boolean is <>;
   with function Eval_Property
     (Node      : Kit_Node'Class; Property : Any_Node_Data_Reference;
      Arguments : Value_Array) return Value_Type is <>;
   with function Kind (Self : Value_Type) return Any_Value_Kind is <>;
   with function As_Text_Type (Self : Value_Type) return Text_Type is <>;
package Wrapping.Input.Kit is

   type Kit_Language_Entity_Type;
   type Kit_Language_Entity is access all Kit_Language_Entity_Type'Class;

   package Kit_Language_Entity_Node_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Kit_Node, Kit_Language_Entity, Hash, "=", "=");
   use Kit_Language_Entity_Node_Maps;

   type Kit_Language_Entity_Type is new Language_Entity_Type with record
      Node : Kit_Node;
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

   procedure Analyze_File (File : String);

   procedure Analyze_Unit (Unit : Analysis_Unit);

private

   type Runtime_Node_Type;
   type Runtime_Node is access all Runtime_Node_Type'Class;

   type Runtime_Node_Type is new Runtime_Object_Type with record
      A_Node : Kit_Node;
   end record;

   function To_Text (Object : Runtime_Node_Type) return Text_Type is (Object.A_Node.Text);

   function To_Text_Expression (Object : access Runtime_Node_Type) return Runtime_Text_Expression is
     (new Runtime_Text_Type'(Value => To_Unbounded_Text (Object.To_Text)));

end Wrapping.Input.Kit;
