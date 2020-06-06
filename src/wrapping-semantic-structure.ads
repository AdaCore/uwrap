with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

package Wrapping.Semantic.Structure is
   -- The purpose is to create the "program" of the wrapping.

   type Entity_Type;
   type Entity is access all Entity_Type'Class;
   package Entity_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Entity);
   use Entity_Maps;
   package Entity_Vectors is new Ada.Containers.Vectors (Positive, Entity);
   use Entity_Vectors;

   type Named_Entity_Type;
   type Named_Entity is access all Named_Entity_Type'Class;
   package Named_Entity_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Named_Entity);
   use Named_Entity_Maps;
   package Named_Entity_Vectors is new Ada.Containers.Vectors (Positive, Named_Entity);
   use Named_Entity_Vectors;

   type Namespace_Type;
   type Namespace is access all Namespace_Type'Class;
   package Namespace_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Namespace);
   use Namespace_Maps;
   package Namespace_Vectors is new Ada.Containers.Vectors (Positive, Namespace);
   use Namespace_Vectors;

   type Module_Type;
   type Module is access all Module_Type'Class;
   package Module_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Module);
   use Module_Maps;
   package Module_Vectors is new Ada.Containers.Vectors (Positive, Module);
   use Module_Vectors;

   type Template_Type;
   type Template is access all Template_Type'Class;
   package Template_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Template);
   use Template_Maps;
   package Template_Vectors is new Ada.Containers.Vectors (Positive, Template);
   use Template_Vectors;

   type Var_Type;
   type Var is access all Var_Type'Class;
   package Var_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Var);
   use Var_Maps;
   package Var_Vectors is new Ada.Containers.Vectors (Positive, Var);
   use Var_Vectors;

   type Weave_Or_Wrap_Type;
   type Weave_Or_Wrap is access all Weave_Or_Wrap_Type'Class;
   package Weave_Or_Wrap_Vectors is new Ada.Containers.Vectors (Positive, Weave_Or_Wrap);
   use Weave_Or_Wrap_Vectors;

   type Command_Type;
   type Command is access all Command_Type'Class;
   package Command_Vectors is new Ada.Containers.Vectors (Positive, Command);
   use Command_Vectors;

   type Command_Function_Type;
   type Command_Function is access all Command_Function_Type'Class;
   package Command_Function_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Command_Function);
   use Command_Function_Maps;
   package Command_Function_Vectors is new Ada.Containers.Vectors (Positive, Command_Function);
   use Command_Function_Vectors;

   type Entity_Type is tagged record
      Node : Template_Node;
      Unit : Analysis_Unit;
      Sloc : Source_Location_Range;

      Parent, Next, Prev : Entity;

      Children_Ordered : Entity_Vectors.Vector;
      Children_Indexed : Entity_Maps.Map;
   end record;

   procedure Add_Child (Parent, Child : access Entity_Type'Class);

   procedure Add_Child (Parent, Child : access Entity_Type'Class; Name_Node : Template_Node'Class);

   procedure Add_Child (Parent, Child : access Entity_Type'Class; Name : Text_Type);

   function Full_Name (An_Entity : Entity_Type) return Text_Type;

   function Find_Visible_Entity (An_Entity : Entity_Type'Class; Name : Text_Type) return Entity;

   type Named_Entity_Type is new Entity_Type with record
      Name_Node : Template_Node;
   end record;

   function Full_Name (An_Entity : Named_Entity_Type) return Text_Type;

   type Namespace_Type is new Entity_Type with record
      null;
   end record;

   type Module_Type is new Entity_Type with record
      Templates_Ordered : Template_Vectors.Vector;
      Templates_Indexed : Template_Maps.Map;

      Modules_Indexed : Module_Maps.Map;
      Modules_Ordered : Module_Vectors.Vector;
      Command_Function_Indexed : Command_Function_Maps.Map;
      Imported_Modules : Module_Maps.Map;
   end record;

   function Resolve_Module_By_Name (Name : Text_Type) return Module;

   type Template_Type is new Named_Entity_Type with record
      Extends : Template;

      Variables_Ordered : Var_Vectors.Vector;
      Variables_Indexed : Var_Maps.Map;
   end record;

   function Instance_Of (Child, Parent : Template) return Boolean;

   function Has_Variable (A_Template : Template_Type; Name : Text_Type) return Boolean;

   function Get_Variable_For_Index (A_Template : Template_Type; Index : Positive) return Var;

   function Get_Component (A_Template : Template_Type; Name : Text_Type) return Entity;

   function Get_Namespace_Prefix (Full_Name : Text_Type; Create_If_Null : Boolean := False) return Namespace;

   type Var_Type_Kind is (Text_Kind, Pattern_Kind);

   type Var_Type is new Named_Entity_Type with record
      Kind : Var_Type_Kind;
      Args : Argument_List;
   end record;

   type Match_Type is new Entity_Type with record
      Matcher_Expression : Template_Node;
   end record;

   type Weave_Or_Wrap_Type is tagged record
      Node : Template_Clause;
      Target_Object    : Template_Node;
      Template_Reference : Template;
      Template_Instance_Expression : Template_Node;

      Arguments  : Argument_List;
   end record;

   type Wrap_Type is new Weave_Or_Wrap_Type with record
      null;
   end record;

   type Weave_Type is new Weave_Or_Wrap_Type with record
      null;
   end record;

   type Apply_Type is new Entity_Type with record
      null;
   end record;

   type Import_Type is new Entity_Type with record
     Imported_Module : Unbounded_Text_Type;
   end record;

   type Command_Type is new Entity_Type with record
      Match_Expression : Template_Node;

      Template_Clause : Weave_Or_Wrap;

      Apply_Expression : Template_Node;
      Nested_Actions   : Entity;
      Else_Actions     : Entity;
   end Record;

   type Command_Function_Type is new Named_Entity_Type with record
      null;
   end record;

end Wrapping.Semantic.Structure;
