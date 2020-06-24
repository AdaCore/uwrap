with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

package Wrapping.Semantic.Structure is
   -- The purpose is to create the "program" of the wrapping.

   type Visit_Action is (Over, Into, Stop, Unknown);

   type T_Entity_Type;
   type T_Entity is access all T_Entity_Type'Class;
   package T_Entity_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, T_Entity);
   use T_Entity_Maps;
   package T_Entity_Vectors is new Ada.Containers.Vectors (Positive, T_Entity);
   use T_Entity_Vectors;

   type T_Named_Entity_Type;
   type T_Named_Entity is access all T_Named_Entity_Type'Class;
   package T_Named_Entity_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, T_Named_Entity);
   use T_Named_Entity_Maps;
   package T_Named_Entity_Vectors is new Ada.Containers.Vectors (Positive, T_Named_Entity);
   use T_Named_Entity_Vectors;

   type T_Namespace_Type;
   type T_Namespace is access all T_Namespace_Type'Class;
   package T_Namespace_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, T_Namespace);
   use T_Namespace_Maps;
   package T_Namespace_Vectors is new Ada.Containers.Vectors (Positive, T_Namespace);
   use T_Namespace_Vectors;

   type T_Module_Type;
   type T_Module is access all T_Module_Type'Class;
   package T_Module_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, T_Module);
   use T_Module_Maps;
   package T_Module_Vectors is new Ada.Containers.Vectors (Positive, T_Module);
   use T_Module_Vectors;

   type T_Template_Type;
   type T_Template is access all T_Template_Type'Class;
   package T_Template_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, T_Template);
   use T_Template_Maps;
   package T_Template_Vectors is new Ada.Containers.Vectors (Positive, T_Template);
   use T_Template_Vectors;

   type T_Var_Type;
   type T_Var is access all T_Var_Type'Class;
   package T_Var_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, T_Var);
   use T_Var_Maps;
   package T_Var_Vectors is new Ada.Containers.Vectors (Positive, T_Var);
   use T_Var_Vectors;

   type T_Weave_Or_Wrap_Type;
   type T_Weave_Or_Wrap is access all T_Weave_Or_Wrap_Type'Class;
   package T_Weave_Or_Wrap_Vectors is new Ada.Containers.Vectors (Positive, T_Weave_Or_Wrap);
   use T_Weave_Or_Wrap_Vectors;

   type T_Command_Type;
   type T_Command is access all T_Command_Type'Class;
   package T_Command_Vectors is new Ada.Containers.Vectors (Positive, T_Command);
   use T_Command_Vectors;

   type T_Visitor_Type;
   type T_Visitor is access all T_Visitor_Type'Class;
   package T_Visitor_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, T_Visitor);
   use T_Visitor_Maps;
   package T_Visitor_Vectors is new Ada.Containers.Vectors (Positive, T_Visitor);
   use T_Visitor_Vectors;

   type T_Entity_Type is tagged record
      Node : Template_Node;
      Unit : Analysis_Unit;
      Sloc : Source_Location_Range;

      Parent, Next, Prev : T_Entity;

      Children_Ordered : T_Entity_Vectors.Vector;
      Children_Indexed : T_Entity_Maps.Map;
   end record;

   procedure Add_Child (Parent, Child : access T_Entity_Type'Class);

   procedure Add_Child (Parent, Child : access T_Entity_Type'Class; Name_Node : Template_Node'Class);

   procedure Add_Child (Parent, Child : access T_Entity_Type'Class; Name : Text_Type);

   function Full_Name (An_Entity : T_Entity_Type) return Text_Type;

   function Find_Visible_Entity (An_Entity : T_Entity_Type'Class; Name : Text_Type) return T_Entity;

   function Get_Variable_For_Index (An_Entity : T_Entity_Type; Index : Positive) return T_Var
   is (null);

   function Get_Component (An_Entity : T_Entity_Type; Name : Text_Type) return T_Entity
   is (null);

   type T_Named_Entity_Type is new T_Entity_Type with record
      Name_Node : Template_Node;
   end record;

   function Full_Name (An_Entity : T_Named_Entity_Type) return Text_Type;

   type T_Namespace_Type is new T_Entity_Type with record
      null;
   end record;

   type T_Module_Type is new T_Entity_Type with record
      Name : Unbounded_Text_Type;

      Templates_Ordered : T_Template_Vectors.Vector;
      Templates_Indexed : T_Template_Maps.Map;

      Variables_Ordered : T_Var_Vectors.Vector;
      Variables_Indexed : T_Var_Maps.Map;

      Visitors_Indexed : T_Visitor_Maps.Map;
      Imported_Modules : T_Module_Maps.Map;
   end record;

   function Resolve_Module_By_Name (Name : Text_Type) return T_Module;

   overriding
   function Full_Name (An_Entity : T_Module_Type) return Text_Type;

   function Get_Variable_For_Index (An_Entity : T_Module_Type; Index : Positive) return T_Var;

   function Get_Component (An_Entity : T_Module_Type; Name : Text_Type) return T_Entity;

   type T_Template_Type is new T_Named_Entity_Type with record
      Extends : T_Template;

      Variables_Ordered : T_Var_Vectors.Vector;
      Variables_Indexed : T_Var_Maps.Map;
   end record;

   function Instance_Of (Child, Parent : T_Template) return Boolean;

   function Has_Variable (A_Template : T_Template_Type; Name : Text_Type) return Boolean;

   function Get_Variable_For_Index (A_Template : T_Template_Type; Index : Positive) return T_Var;

   function Get_Component (A_Template : T_Template_Type; Name : Text_Type) return T_Entity;

   function Get_Namespace_Prefix (Full_Name : Text_Type; Create_If_Null : Boolean := False) return T_Namespace;

   type Var_Type_Kind is (Text_Kind, Set_Kind, Map_Kind, Pattern_Kind);

   type T_Var_Type is new T_Named_Entity_Type with record
      Kind : Var_Type_Kind;
      Args : Argument_List;
   end record;

   type Match_Type is new T_Entity_Type with record
      Matcher_Expression : Template_Node;
   end record;

   type T_Weave_Or_Wrap_Type is tagged record
      Node                         : Template_Clause;
      Is_All                       : Boolean := False;
      Is_Null                      : Boolean := False;
      Target_Object                : Template_Node;
      Call_Reference               : T_Entity;
      Arguments                    : Argument_List;
      A_Visit_Action               : Visit_Action := Unknown;
   end record;

   type Wrap_Type is new T_Weave_Or_Wrap_Type with record
      null;
   end record;

   type Weave_Type is new T_Weave_Or_Wrap_Type with record
      null;
   end record;

   type Apply_Type is new T_Entity_Type with record
      null;
   end record;

   type Import_Type is new T_Entity_Type with record
     Imported_Module : Unbounded_Text_Type;
   end record;

   type T_Command_Type is new T_Entity_Type with record
      Match_Expression : Template_Node;

      Template_Clause : T_Weave_Or_Wrap;

      Traverse_Expression : Template_Node;
      Nested_Actions   : T_Entity;
      Else_Actions     : T_Entity;
   end Record;

   type T_Visitor_Type is new T_Named_Entity_Type with record
      Arguments_Ordered : T_Var_Vectors.Vector;
      Arguments_Indexed : T_Var_Maps.Map;
   end record;

end Wrapping.Semantic.Structure;
