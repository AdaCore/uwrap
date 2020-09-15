------------------------------------------------------------------------------
--                                                                          --
--                                  UWrap                                   --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- UWrap is free software;  you can  redistribute it  and/or modify it      --
-- under terms of the  GNU General Public License as published  by the Free --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version.  UWrap is distributed in the hope that it will be useful, but   --
-- WITHOUT ANY WARRANTY;  without even the  implied warranty of MERCHANTA-  --
-- BILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public  --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General Public License  distributed with UWrap; see file COPYING3.  If --
-- not, go to http://www.gnu.org/licenses for a complete copy of the        --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;
with Libtemplatelang.Common;   use Libtemplatelang.Common;

with Wrapping.Utils; use Wrapping.Utils;

package Wrapping.Semantic.Structure is
   --  The purpose is to create the "program" of the wrapping.

   type Visit_Action is (Over, Into, Into_Override_Anchor, Stop, Unknown);

   type T_Entity_Type;
   type T_Entity is access all T_Entity_Type'Class;
   package T_Entity_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, T_Entity);
   use T_Entity_Maps;
   package T_Entity_Vectors is new Ada.Containers.Vectors (Positive, T_Entity);
   use T_Entity_Vectors;

   type T_Named_Entity_Type;
   type T_Named_Entity is access all T_Named_Entity_Type'Class;
   package T_Named_Entity_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, T_Named_Entity);
   use T_Named_Entity_Maps;
   package T_Named_Entity_Vectors is new Ada.Containers.Vectors
     (Positive, T_Named_Entity);
   use T_Named_Entity_Vectors;

   type T_Namespace_Type;
   type T_Namespace is access all T_Namespace_Type'Class;
   package T_Namespace_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, T_Namespace);
   use T_Namespace_Maps;
   package T_Namespace_Vectors is new Ada.Containers.Vectors
     (Positive, T_Namespace);
   use T_Namespace_Vectors;

   type T_Module_Type;
   type T_Module is access all T_Module_Type'Class;
   package T_Module_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, T_Module);
   use T_Module_Maps;
   package T_Module_Vectors is new Ada.Containers.Vectors (Positive, T_Module);
   use T_Module_Vectors;

   type T_Template_Type;
   type T_Template is access all T_Template_Type'Class;
   package T_Template_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, T_Template);
   use T_Template_Maps;
   package T_Template_Vectors is new Ada.Containers.Vectors
     (Positive, T_Template);
   use T_Template_Vectors;

   type T_Var_Type;
   type T_Var is access all T_Var_Type'Class;
   package T_Var_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, T_Var);
   use T_Var_Maps;
   package T_Var_Vectors is new Ada.Containers.Vectors (Positive, T_Var);
   use T_Var_Vectors;

   type T_Command_Sequence_Type;
   type T_Command_Sequence is access all T_Command_Sequence_Type'Class;
   package Command_Sequence_Vectors is new Ada.Containers.Vectors
     (Positive, T_Command_Sequence);
   use Command_Sequence_Vectors;

   type T_Command_Sequence_Element_Type;
   type T_Command_Sequence_Element is
     access all T_Command_Sequence_Element_Type'Class;
   package Command_Sequence_Element_Vectors is new Ada.Containers.Vectors
     (Positive, T_Command_Sequence_Element);
   use Command_Sequence_Element_Vectors;

   type T_Template_Call_Type;
   type T_Template_Call is access all T_Template_Call_Type'Class;

   type T_Template_Section_Type;
   type T_Template_Section is access all T_Template_Section_Type'Class;
   package T_Template_Section_Vectors is new Ada.Containers.Vectors
     (Positive, T_Template_Section);
   use T_Template_Section_Vectors;

   type T_Command_Type;
   type T_Command is access all T_Command_Type'Class;
   package T_Command_Vectors is new Ada.Containers.Vectors
     (Positive, T_Command);
   use T_Command_Vectors;

   type T_Function_Type;
   type T_Function is access all T_Function_Type'Class;
   package T_Function_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, T_Function);
   use T_Function_Maps;
   package T_Function_Vectors is new Ada.Containers.Vectors
     (Positive, T_Function);
   use T_Function_Vectors;

   type T_Expr_Type (Kind : Template_Node_Kind_Type);
   type T_Expr is access all T_Expr_Type'Class;
   package T_Expr_Vectors is new Ada.Containers.Vectors (Positive, T_Expr);
   use T_Expr_Vectors;
   package T_Expr_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, T_Expr);
   use T_Expr_Maps;

   type T_Arg_Type;
   type T_Arg is access all T_Arg_Type'Class;
   package T_Arg_Vectors is new Ada.Containers.Vectors (Positive, T_Arg);

   type T_Create_Tree_Type;
   type T_Create_Tree is access all T_Create_Tree_Type'Class;
   package T_Create_Tree_Vectors is new Ada.Containers.Vectors
     (Positive, T_Create_Tree);

   type T_Entity_Type is tagged record
      Node : Template_Node;
      Unit : Analysis_Unit;
      Sloc : Source_Location_Range;

      Parent, Next, Prev : T_Entity;

      Children_Ordered : T_Entity_Vectors.Vector;
      Children_Indexed : T_Entity_Maps.Map;
   end record;

   procedure Add_Child (Parent, Child : access T_Entity_Type'Class);

   procedure Add_Child
     (Parent, Child : access T_Entity_Type'Class;
      Name_Node     : Template_Node'Class);

   procedure Add_Child
     (Parent, Child : access T_Entity_Type'Class; Name : Text_Type);

   function Full_Name (An_Entity : T_Entity_Type) return Text_Type;

   function Find_Visible_Entity
     (An_Entity : T_Entity_Type'Class; Name : Text_Type) return T_Entity;

   function Get_Component
     (An_Entity : T_Entity_Type; Name : Text_Type) return T_Entity is
     (null);

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

      Imported_Modules : T_Module_Maps.Map;
   end record;

   function Resolve_Module_By_Name (Name : Text_Type) return T_Module;

   overriding function Full_Name (An_Entity : T_Module_Type) return Text_Type;

   function Get_Component
     (An_Entity : T_Module_Type; Name : Text_Type) return T_Entity;

   overriding procedure Resolve_References (An_Entity : access T_Module_Type);

   type T_Template_Type is new T_Named_Entity_Type with record
      Extends : T_Template;
      Program : T_Command;
   end record;

   function Instance_Of (Child, Parent : T_Template) return Boolean;

   function Get_Component
     (A_Template : T_Template_Type; Name : Text_Type) return T_Entity;

   function Get_Namespace_Prefix
     (Full_Name : Text_Type; Create_If_Null : Boolean := False)
      return T_Namespace;

   overriding procedure Resolve_References
     (An_Entity : access T_Template_Type);

   type Var_Type_Kind is
     (Object_Kind, Integer_Kind, Text_Kind, Set_Kind, Map_Kind, Vector_Kind);

   type T_Var_Type is new T_Named_Entity_Type with record
      Kind      : Var_Type_Kind;
      Args      : T_Arg_Vectors.Vector;
      Init_Expr : T_Expr;
   end record;

   type Match_Type is new T_Entity_Type with record
      Matcher_Expression : T_Expr;
   end record;

   type T_Command_Sequence_Type is new T_Entity_Type with record
      First_Element : T_Command_Sequence_Element;
   end record;

   type T_Command_Sequence_Element_Type is new T_Entity_Type with record
      Is_Else : Boolean;
      --  True if this element is an else or elsmatch element

      Match_Expression : T_Expr;
      --  If this element is an elmatch element, provide the expression to
      --  match against

      Vars         : T_Var_Vectors.Vector;
      Commands     : T_Command_Vectors.Vector;
      Next_Element : T_Command_Sequence_Element;
   end record;

   type T_Template_Call_Type is new T_Entity_Type with record
      Is_Null       : Boolean := False;
      Captured_Name : Unbounded_Text_Type;
      Reference     : T_Entity;
      Args          : T_Arg_Vectors.Vector;
   end record;

   overriding procedure Resolve_References
     (An_Entity : access T_Template_Call_Type);

   type Template_Section_Kind is (Wrap_Kind, Weave_Kind, Walk_Kind);

   type T_Template_Section_Type is new T_Entity_Type with record
      Call           : T_Template_Call;
      Kind           : Template_Section_Kind;
      A_Visit_Action : Visit_Action := Unknown;
   end record;

   type Import_Type is new T_Entity_Type with record
      Imported_Module : Unbounded_Text_Type;
   end record;

   type T_Command_Type is new T_Entity_Type with record
      Defer            : Boolean;
      Defer_Expression : T_Expr;
      Deferred_Closure : Text_Sets.Set;

      Match_Expression : T_Expr;
      Pick_Expression  : T_Expr;
      Template_Section : T_Template_Section;

      Command_Sequence : T_Command_Sequence;
   end record;

   overriding procedure Resolve_References (An_Entity : access T_Command_Type);

   type T_Function_Type is new T_Named_Entity_Type with record
      Arguments_Ordered : T_Var_Vectors.Vector;
      Arguments_Indexed : T_Var_Maps.Map;

      Program : T_Command_Sequence;
   end record;

   type String_Part_Kind is (Str_Kind, Expr_Kind, Group_Kind);

   type String_Kind is
     (String_Simple, String_Raw, String_Indent, String_Regexp);

   type String_Part (Kind : String_Part_Kind := Str_Kind) is record
      Offset_Line, Offset_Column : Integer;
      Indent : Integer;

      case Kind is
         when Str_Kind =>
            Value : Unbounded_Text_Type;

         when Expr_Kind =>
            Expr   : T_Expr;

         when Group_Kind =>
            Group_Number : Integer;

      end case;
   end record;

   package Processed_String_Vector is new Ada.Containers.Vectors
     (Positive, String_Part);
   use Processed_String_Vector;

   type T_Expr_Type (Kind : Template_Node_Kind_Type) is new T_Entity_Type with
   record
      Has_New : Boolean := False;

      case Kind is
         when Template_Match_Capture =>
            Match_Capture_Expr : T_Expr;

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
            Str_Kind : String_Kind;
            Str      : Processed_String_Vector.Vector;

         when Template_Call_Expr =>
            Called : T_Expr;
            Args   : T_Arg_Vectors.Vector;

         when Template_Defer_Expr =>
            Deferred_Expr    : T_Expr;
            Deferred_Closure : Text_Sets.Set;

         when Template_New_Expr =>
            Tree : T_Create_Tree;

         when Template_At_Ref =>
            null;

         when Template_Qualified_Match =>
            Qualified_Match_Expr : T_Expr;

         when Template_Fold_Expr =>
            Default   : T_Expr;
            Combine   : T_Expr;
            Separator : T_Expr;

         when Template_All_Expr =>
            All_Prefix : T_Expr;
            All_Match  : T_Expr;

         when Template_Reg_Expr =>
            Reg_Expr_Left  : T_Expr;
            Reg_Expr_Right : T_Expr;

         when Template_Reg_Expr_Anchor =>
            null;

         when Template_Reg_Expr_Quantifier =>
            Quantifier_Expr : T_Expr;
            Min             : Integer;
            Max             : Integer;

         when Template_Match_Expr =>
            Match_Match_Expr : T_Expr;
            Match_Pick_Expr  : T_Expr;
            Match_Else_Expr  : T_Expr;

         when Template_Filter_Expr =>
            Filter_Expr : T_Expr;

         when others =>
            null;
      end case;
   end record;

   overriding procedure Resolve_References (An_Entity : access T_Expr_Type);

   type T_Arg_Type is new T_Entity_Type with record
      Name_Node : Identifier;
      Name      : Unbounded_Text_Type;
      Expr      : T_Expr;
   end record;

   type T_Create_Tree_Type is new T_Entity_Type with record
      Call    : T_Template_Call;
      Subtree : T_Create_Tree_Vectors.Vector;
   end record;

end Wrapping.Semantic.Structure;
