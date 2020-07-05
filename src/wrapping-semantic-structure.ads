with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;
with Libtemplatelang.Common; use Libtemplatelang.Common;

with Wrapping.Utils; use Wrapping.Utils;

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

   type T_Command_Sequence_Type;
   type T_Command_Sequence is access all T_Command_Sequence_Type'Class;
   package Command_Sequence_Vectors is new Ada.Containers.Vectors (Positive, T_Command_Sequence);
   use Command_Sequence_Vectors;

   type T_Template_Call_Type;
   type T_Template_Call is access all T_Template_Call_Type'Class;

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

   type T_Expr_Type (Kind : Template_Node_Kind_Type);
   type T_Expr is access all T_Expr_Type'Class;
   package T_Expr_Vectors is new Ada.Containers.Vectors (Positive, T_Expr);

   type T_Arg_Type;
   type T_Arg is access all T_Arg_Type'Class;
   package T_Arg_Vectors is new Ada.Containers.Vectors (Positive, T_Arg);

   type T_Create_Tree_Type;
   type T_Create_Tree is access all T_Create_Tree_Type'Class;
   package T_Create_Tree_Vectors is new Ada.Containers.Vectors (Positive, T_Create_Tree);

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

   procedure Resolve_References (An_Entity : access T_Entity_Type);

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

   overriding
   procedure Resolve_References (An_Entity : access T_Module_Type);

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

   overriding
   procedure Resolve_References (An_Entity : access T_Template_Type);

   type Var_Type_Kind is (Text_Kind, Set_Kind, Map_Kind, Pattern_Kind);

   type T_Var_Type is new T_Named_Entity_Type with record
      Kind : Var_Type_Kind;
      Args : T_Arg_Vectors.Vector;
   end record;

   type Match_Type is new T_Entity_Type with record
      Matcher_Expression : T_Expr;
   end record;

   type T_Command_Sequence_Type is new T_Entity_Type with record
      Current       : T_Command;
      Next_Sequence : T_Command_Sequence;
   end record;

   type T_Template_Call_Type is new T_Entity_Type with record
      Is_Null        : Boolean := False;
      Captured_Name : Unbounded_Text_Type;
      Reference     : T_Entity;
      Args          : T_Arg_Vectors.Vector;
   end record;

   overriding
   procedure Resolve_References (An_Entity : access T_Template_Call_Type);

   type T_Weave_Or_Wrap_Type is new T_Entity_Type with record
      Call           : T_Template_Call;
      A_Visit_Action : Visit_Action := Unknown;
   end record;

   type Wrap_Type is new T_Weave_Or_Wrap_Type with record
      null;
   end record;

   type Weave_Type is new T_Weave_Or_Wrap_Type with record
      null;
   end record;

   type Import_Type is new T_Entity_Type with record
     Imported_Module : Unbounded_Text_Type;
   end record;

   type T_Command_Type is new T_Entity_Type with record
      Match_Expression : T_Expr;
      Pick_Expression  : T_Expr;
      Template_Section : T_Weave_Or_Wrap;

      Nested_Actions   : T_Entity;
      Command_Sequence : T_Command_Sequence;
      Else_Actions     : T_Command;
   end Record;

   overriding
   procedure Resolve_References (An_Entity : access T_Command_Type);

   type T_Visitor_Type is new T_Named_Entity_Type with record
      Arguments_Ordered : T_Var_Vectors.Vector;
      Arguments_Indexed : T_Var_Maps.Map;
   end record;

   type String_Part_Kind is (Str_Kind, Expr_Kind, Group_Kind);

   type String_Part (Kind : String_Part_Kind := Str_Kind) is record
      Offset_Line, Offset_Column : Integer;

      case Kind is
         when Str_Kind =>
            Value : Unbounded_Text_Type;

         when Expr_Kind =>
            Expr : T_Expr;

         when Group_Kind =>
            Group_Number : Integer;

      end case;
   end record;

   package Processed_String_Vector is new Ada.Containers.Vectors (Positive, String_Part);
   use Processed_String_Vector;

   type T_Expr_Type (Kind : Template_Node_Kind_Type) is new T_Entity_Type with record
      Has_New : Boolean := False;

      case Kind is
         when Template_Match_Capture =>
            Match_Expr : T_Expr;

         when Template_Selector =>
            Selector_Left, Selector_Right : T_Expr;

         when Template_Binary_Expr =>
            Binary_Left, Binary_Right : T_Expr;

         when Template_Unary_Expr =>
            Unary_Right : T_Expr;

         when Template_Literal =>
            null;

         when Template_Token_Identifier | Template_Identifier =>
            Static_Reference : T_Entity;

         when Template_Number =>
            Number : Integer;

         when Template_Str =>
            Str : Processed_String_Vector.Vector;

         when Template_Call_Expr =>
            Called : T_Expr;
            Args   : T_Arg_Vectors.Vector;

         when Template_Lambda_Expr =>
            Lambda_Expr : T_Expr;
            Lambda_Closure : Text_Sets.Set;

         when Template_New_Expr =>
            Tree : T_Create_Tree;

         when Template_At_Ref =>
            null;

         when Template_Qualified_Match =>
            Qualified_Match_Expr : T_Expr;

         when Template_Fold_Expr =>
            Default     : T_Expr;
            Combine     : T_Expr;

         when Template_All_Expr =>
            All_Prefix : T_Expr;
            All_Match  : T_Expr;

         when others =>
            null;
      end case;
   end record;

   overriding
   procedure Resolve_References (An_Entity : access T_Expr_Type);

   type T_Arg_Type is new T_Entity_Type with record
      Name_Node : Template_Node;
      Name : Unbounded_Text_Type;
      Expr : T_Expr;
   end record;

   type T_Create_Tree_Type is new T_Entity_Type with record
      Call    : T_Template_Call;
      Subtree : T_Create_Tree_Vectors.Vector;
   end record;

end Wrapping.Semantic.Structure;
