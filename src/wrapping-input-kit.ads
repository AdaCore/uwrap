with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;

generic
   Language_Name : in Text_Type;

   type Kit_Node is tagged private;
   type Kit_Unit is tagged private;
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
   type Value_Constraint is private;
   type Value_Constraint_Array is
     array (Positive range <>) of Value_Constraint;
   type Token_Reference is private;
   type Token_Data_Type is private;
   type Token_Kind is (<>);

   None : Any_Node_Data_Reference;
   No_Kit_Node : Kit_Node;
   Default_Grammar_Rule : Grammar_Rule;
   Default_Charset : String;
   No_Unit_Provider_Reference : Unit_Provider_Reference;
   No_Node_Type_Id : Any_Node_Type_Id;

   Text_Type_Value : Any_Value_Kind;
   Node_Value : Any_Value_Kind;
   Boolean_Value : Any_Value_Kind;
   No_Token : Token_Reference;

   with function Children (Node : Kit_Node'Class) return Kit_Node_Array is <>;
   with function Parent (Node : Kit_Node'Class) return Kit_Node is <>;
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
   with function As_Node (Self : Value_Type) return Kit_Node is <>;
   with function As_Boolean (Self : Value_Type) return Boolean is <>;
   with function Property_Argument_Types
     (Property : Any_Node_Data_Reference) return Value_Constraint_Array is <>;
   with function Property_Argument_Default_Value
     (Property : Any_Node_Data_Reference; Argument_Number : Positive)
      return Value_Type is <>;
   with function Full_Sloc_Image (Node : Kit_Node'Class) return Text_Type is <>;
   with function Sloc_Range (Node : Kit_Node'Class) return Source_Location_Range is <>;
   with function Get_Filename (Unit : Kit_Unit'Class) return String is <>;
   with function Unit (Node : Kit_Node'Class) return Kit_Unit is <>;
   with function Token_Start (Node : Kit_Node'Class) return Token_Reference is <>;
   with function Token_End (Node : Kit_Node'Class) return Token_Reference is <>;
   with function Next
     (Token : Token_Reference; Exclude_Trivia : Boolean := False)
      return Token_Reference is <>;
   with function Previous
     (Token : Token_Reference; Exclude_Trivia : Boolean := False)
      return Token_Reference is <>;
   with function Data (Token : Token_Reference) return Token_Data_Type is <>;
   with function Text (Token : Token_Reference) return Text_Type is <>;
   with function Kind (Token_Data : Token_Data_Type) return Token_Kind is <>;
   with function Is_Trivia (Token : Token_Reference) return Boolean is <>;
package Wrapping.Input.Kit is

   type W_Kit_Node_Type;
   type W_Kit_Node is access all W_Kit_Node_Type'Class;

   type W_Kit_Node_Token_Type;
   type W_Kit_Node_Token is access all W_Kit_Node_Token_Type'Class;

   procedure Analyze_File (File : String);

   procedure Analyze_Unit (Unit : Analysis_Unit);

   ----------------
   -- W_Kit_Node --
   ----------------

   function Lt (Left, Right : Kit_Node) return Boolean;

   function Eq (Left, Right : W_Kit_Node) return Boolean;

   package W_Kit_Node_Entity_Node_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Kit_Node, W_Kit_Node, Lt, Eq);
   use W_Kit_Node_Entity_Node_Maps;

   type W_Kit_Node_Type is new W_Node_Type with record
      Node : Kit_Node;
      Children_Computed : Boolean := False;
      Children_By_Node : W_Kit_Node_Entity_Node_Maps.Map;

      First_Token_Node : W_Kit_Node_Token;
      First_Token_Node_Computed : Boolean := False;
   end record;

   overriding
   procedure Pre_Visit (An_Entity : access W_Kit_Node_Type);

   overriding
   function Push_Value
     (An_Entity : access W_Kit_Node_Type;
      Name      : Text_Type) return Boolean;

   overriding
   function To_String (Object : W_Kit_Node_Type) return Text_Type;

   overriding
   function To_Debug_String (Object : W_Kit_Node_Type) return Text_Type;

   overriding
   function Language (An_Entity : W_Kit_Node_Type) return Text_Type is (Language_Name);

   ----------------------
   -- W_Kit_Token_Node --
   ----------------------

   type W_Kit_Node_Token_Type is new W_Node_Type with record
      Node : Token_Reference;
      Next_Computed : Boolean := False;
   end record;

   overriding
   procedure Pre_Visit (An_Entity : access W_Kit_Node_Token_Type);

   overriding
   function Push_Value
     (An_Entity : access W_Kit_Node_Token_Type;
      Name      : Text_Type) return Boolean;

   overriding
   function To_String (Object : W_Kit_Node_Token_Type) return Text_Type;

   overriding
   function To_Debug_String (Object : W_Kit_Node_Token_Type) return Text_Type;

   overriding
   function Language (An_Entity : W_Kit_Node_Token_Type) return Text_Type is (Language_Name & "_token");
private

   type W_Source_Node_Type;
   type W_Source_Node is access all W_Source_Node_Type'Class;

   type W_Source_Node_Type is new W_Object_Type with record
      A_Node : Kit_Node;
   end record;

   overriding
   function To_String (Object : W_Source_Node_Type) return Text_Type is (Object.A_Node.Text);

end Wrapping.Input.Kit;
