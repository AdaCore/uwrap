with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
use Ada.Containers;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libtemplatelang.Analysis;

with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Libtemplatelang;

package Wrapping.Input.Ada is

   procedure Register_Globals;

   procedure Populate_Language_Entities (Root : Ada_Node);

   type Ada_Language_Entity_Type;
   type Ada_Language_Entity is access all Ada_Language_Entity_Type'Class;
   package Ada_Language_Entity_Maps is new Standard.Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Ada_Language_Entity);
   use Ada_Language_Entity_Maps;
   package Ada_Language_Entity_Vectors is new Standard.Ada.Containers.Vectors (Positive, Ada_Language_Entity);
   use Ada_Language_Entity_Vectors;

   -------------------------
   -- Ada_Language_Entity --
   -------------------------

   type Ada_Language_Entity_Type is new Language_Entity_Type with record
      Node : Ada_Node;
      Name : Ada_Language_Entity;
   end record;

   overriding
   function Push_Value
     (An_Entity : access Ada_Language_Entity_Type;
      Name      : Text_Type) return Boolean;

   overriding
   function Push_Call_Result
     (An_Entity : access Ada_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   overriding
   function Push_Match_Result
     (An_Entity : access Ada_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   overriding
   function To_Text (An_Entity : Ada_Language_Entity_Type) return Text_Type;

   type Unit_Language_Entity_Type is new Ada_Language_Entity_Type with record
      null;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access Unit_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   type Name_Language_Entity_Type is new Ada_Language_Entity_Type with record
      null;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access Name_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   type Package_Language_Entity_Type is new Ada_Language_Entity_Type with record
      null;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access Package_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   type With_Language_Entity_Type is new Ada_Language_Entity_Type with record
      null;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access With_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   type Use_Language_Entity_Type is new Ada_Language_Entity_Type with record
      null;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access Use_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   type Subprogram_Language_Entity_Type is new Ada_Language_Entity_Type with record
      null;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access Subprogram_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   type Parameter_Language_Entity_Type is new Ada_Language_Entity_Type with record
      Type_Name : Ada_Language_Entity;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access Parameter_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   overriding
   function Push_Value
     (An_Entity : access Parameter_Language_Entity_Type;
      Name      : Text_Type) return Boolean;

   type Return_Type_Language_Entity_Type is new Ada_Language_Entity_Type with record
      Type_Name : Ada_Language_Entity;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access Return_Type_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   overriding
   function Push_Value
     (An_Entity : access Return_Type_Language_Entity_Type;
      Name      : Text_Type) return Boolean;

   type Type_Language_Entity_Type is new Ada_Language_Entity_Type with record
      null;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access Type_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   type Object_Language_Entity_Type is new Ada_Language_Entity_Type with record
      Type_Name : Ada_Language_Entity;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access Object_Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   overriding
   function Push_Value
     (An_Entity : access Object_Language_Entity_Type;
      Name      : Text_Type) return Boolean;

end Wrapping.Input.Ada;
