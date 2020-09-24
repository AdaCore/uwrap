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
-- General Public License  distributed with UWrap; see file COPYING3.  If   --
-- not, go to http://www.gnu.org/licenses for a complete copy of the        --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the types necessary to describe a UWrap program
--  together with the primitives associated to those types.

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;
with Libtemplatelang.Common;   use Libtemplatelang.Common;

with Wrapping.Utils; use Wrapping.Utils;

package Wrapping.Semantic.Structure is

   type Visit_Action is (Over, Into, Into_Override_Anchor, Stop, Unknown);
   type Visit_Action_Ptr is access all Visit_Action;

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
      --  Template node that corresponds to this entity

      Unit : Analysis_Unit;
      --  Unit in which this template node is declared.

      Sloc : Source_Location_Range;
      --  Source location at which this template node is declared

      Parent, Next, Prev : T_Entity;
      --  Parent, Next and Prev nodes created around this entity

      Children_Ordered : T_Entity_Vectors.Vector;
      --  Children of this entity, in the order they were created

      Children_Indexed : T_Entity_Maps.Map;
      --  Children of this entity ordered by name - excluding children that are
      --  not associated with a name
   end record;
   --  This type is refering to a node coming from the UWrap programming
   --  language. It's used to post process the output of the parser and create
   --  additional data and analysis necessary to represent a full UWrap
   --  program. For example, it will hold name resolution or pre-computed
   --  results. Not all nodes from the UWrap language lead to T_Entities, only
   --  those that are ultimately necessary to run the prorgam and require ahead
   --  of time analysis. Some T_Entities may also be created as an expansion of
   --  the input program without a correspionding template node.

   procedure Add_Child (Parent, Child : access T_Entity_Type'Class);
   --  Connect a child to a parent

   procedure Add_Child
     (Parent, Child : access T_Entity_Type'Class;
      Name_Node     : Template_Node'Class);
   --  Connect a named child to a parent retreiving the name from a node

   procedure Add_Child
     (Parent, Child : access T_Entity_Type'Class; Name : Text_Type);
   --  Connect a named child to a parent retreiving the name from text

   function Full_Name (An_Entity : T_Entity_Type) return Text_Type;
   --  Return the full name of the entity in parameter, prefixed by the
   --  module names and other named lexical scopes.

   procedure Resolve_References (An_Entity : access T_Entity_Type);
   --  If this entity requires reference resolution, e.g. name resolution,
   --  computes it here. This requires that the whole structure of T_Entity has
   --  been created before. Also calls Resolve_References on children.

   type T_Named_Entity_Type is new T_Entity_Type with record
      Name_Node : Template_Node;
   end record;
   --  Base type for named entities whose name is coming from a node.
   --  TODO: we're using the node as a way to avoid storing a whole string,
   --  should be using a symbol instead.

   overriding function Full_Name
     (An_Entity : T_Named_Entity_Type) return Text_Type;
   --  See parent documentation

   type T_Namespace_Type is new T_Entity_Type with record
      null;
   end record;
   --  A namespace is an entity used to store either other namespaces or
   --  modules. It does not correspond to Template_Node, but instead serve as
   --  a way to store the directory structure. For example, in:
   --     a/b/c.wrp
   --  Three namespace are created: the anonymous root one, the one for the
   --  directory a and the one for the directory b. c.wrp is a module in that
   --  last namespace.

   type T_Module_Type is new T_Entity_Type with record
      Name : Unbounded_Text_Type;
      --  The simple name of the module (without the prefixes)

      Templates_Ordered : T_Template_Vectors.Vector;
      --  Templates declared in this module, in order of appearance.

      Templates_Indexed : T_Template_Maps.Map;
      --  Templates declared in this module, by name

      Variables_Ordered : T_Var_Vectors.Vector;
      --  Global variables declared in this module, in order of appearance.

      Variables_Indexed : T_Var_Maps.Map;
      --  Global variables declared in this module, by name

      Imported_Modules : T_Module_Maps.Map;
      --  Module imported by import clauses
   end record;
   --  A module is the basic architecture unit of a UWrap. It's a file that
   --  contains a program. There's no specific language construction that maps
   --  to a module other than the fact that each UWrap file corresponds to
   --  a module, named after the base name of that file. A module belong
   --  to a hierarchy of namespaces that maps to the hierarchy of directories
   --  from one of the root directory to this module. E.g. in:
   --     a/b/c.wrp
   --  c is a module declared in c.wrp, in the namespace a.b. Full name of this
   --  module is a.b.c

   function Resolve_Module_By_Name (Full_Name : Text_Type) return T_Module;
   --  Returns the model references that corresponds to the module name in
   --  parameter (needs to be the full dotted name), null if not found.

   overriding function Full_Name (An_Entity : T_Module_Type) return Text_Type;
   --  Returns the full dotted name for this module

   overriding procedure Resolve_References (An_Entity : access T_Module_Type);
   --  Resolves the references to import clauses and children entities

   type T_Template_Type is new T_Named_Entity_Type with record
      Extends : T_Template;
      --  Reference to the parent template, null if none.

      Program : T_Command;
      --  A template is essentially a command storing its variable. This points
      --  to te program of that command
   end record;
   --  Models a template declaration

   function Instance_Of (Child, Parent : T_Template) return Boolean;
   --  Returns true if Child and Parent are the same entity, or if Child is a
   --  direct or indirect child of parent.

   function Get_Namespace_Prefix_For_Module
     (Full_Name : Text_Type; Create_If_Null : Boolean := False)
      return T_Namespace;
   --  Given a module name a.b.c, return a reference to its namespace a.b,
   --  create non yet registered namespaces if Create_If_Null is true.

   overriding procedure Resolve_References
     (An_Entity : access T_Template_Type);
   --  Resolves the reference to the extended template and child entities.

   type Var_Type_Kind is
     (Object_Kind, Integer_Kind, Text_Kind, Set_Kind, Map_Kind, Vector_Kind);
   --  Models various primitive types available for variable.

   type T_Var_Type is new T_Named_Entity_Type with record
      Kind      : Var_Type_Kind;
      --  The kind of this variable

      Args      : T_Arg_Vectors.Vector;
      --  Some types may be parametrized, e.g. vector (object).
      --  TODO: This infromation isn't used at this stage as there's little to
      --  no type checking yet implemented.

      Init_Expr : T_Expr;
      --  Expression to evaluate for the initial value of this type, e.g. in:
      --     var x => <some initial value>;
   end record;
   --  Refers to a variable declaration, either global or within a command

   type T_Command_Sequence_Type is new T_Entity_Type with record
      First_Element : T_Command_Sequence_Element;
      --  Sequences are modeled through a series of element separated by either
      --  then, elsmatch or else delimiters.
   end record;
   --  Models a command sequence introduced by a "do" and terminated by and
   --  "end".

   type T_Command_Sequence_Element_Type is new T_Entity_Type with record
      Is_Else : Boolean;
      --  True if this element is an else or elsmatch element

      Match_Expression : T_Expr;
      --  If this element is an elmatch element, provide the expression to
      --  match against

      Vars         : T_Var_Vectors.Vector;
      --  Variables declared in this element. Variables will be evaluated from
      --  first to last.

      Commands     : T_Command_Vectors.Vector;
      --  Commands declared in this element. As with other commands, within a
      --  sequence element, commands will be evaluated from last to first.

      Next_Element : T_Command_Sequence_Element;
      --  If not null, points to the next element in the sequence
   end record;
   --  A sequence element is an element in a do .. end sequence, separated by
   --  a then, elsematch or else. E.g. in:
   --     match <some expression>
   --     do
   --        <some commands>
   --     then
   --        <some commands>
   --     else
   --        <some commands>
   --     then
   --        <some commands>
   --     end;
   --  Four command elements will be created. The execution code will know to
   --  either execute the first two or the last two depending on the result
   --  of the matcher.

   type T_Template_Call_Type is new T_Entity_Type with record
      Is_Null       : Boolean := False;
      --  If true, we're in a call of the form:
      --     wrap null (some_template);
      --  the expected behavior is to forbid that template type to wrap the
      --  wrapped object.

      Captured_Name : Unbounded_Text_Type;
      --  Template calls may have a captured name, in order to be able to refer
      --  to the newly created template in the process of its parameter
      --  evaluation. For example in:
      --     wrap x: some_template (a => x.b, b => "something");
      --  This field captures the name of that variable if any, empty if none

      Reference     : T_Template;
      --  Reference to the template that this call will designate.

      Args          : T_Arg_Vectors.Vector;
      --  Arguments passed to this template call
   end record;
   --  Models a template call as found in template clauses (wrap / weave /
   --  walk) as well as allocated through new.

   overriding procedure Resolve_References
     (An_Entity : access T_Template_Call_Type);
   --  Checks the legality of the call and resolves the template reference.

   type Template_Section_Kind is (Wrap_Kind, Weave_Kind, Walk_Kind);
   --  List all ways to incoke a template in a template section of a command

   type T_Template_Section_Type is new T_Entity_Type with record
      Call           : T_Template_Call;
      --  Refers to the template call of this template section

      Kind           : Template_Section_Kind;
      --  The kind of operation to apply (wrap, weave or walk).

      A_Visit_Action : Visit_Action := Unknown;
      --  Template clauses can also specify a visit action, as in:
      --     wrap over;
      --  In this case there's no call but a specific visit action
   end record;
   --  Models the template section of a command, for example:
   --     wrap some_template ();

   type T_Command_Type is new T_Entity_Type with record
      Defer            : Boolean;
      --  True if this command is a deferred command.

      Defer_Expression : T_Expr;
      --  If the command is a deferred command, it may come with an expression
      --  describing under which circumstances it should be executed. This
      --  expression is stored in this field.

      Deferred_Closure : Text_Sets.Set;
      --  In the case of a defered commands, list the symbols to capture when
      --  scheduling.

      Match_Expression : T_Expr;
      --  If this command has a match section, references its expression.

      Pick_Expression  : T_Expr;
      --  If this command has a pick section, references its expression.

      Template_Section : T_Template_Section;
      --  If this command has a template section, references it.

      Command_Sequence : T_Command_Sequence;
      --  If this command introduces a command sequence instead of a template
      --  section, references it.
   end record;
   --  This type models a command as a whole. It can be used as a list of
   --  commands, or as the command defining a template or a function.

   overriding procedure Resolve_References (An_Entity : access T_Command_Type);
   --  Resolves the closure of the command if needed, and resolve child
   --  references

   type T_Function_Type is new T_Named_Entity_Type with record
      Arguments_Ordered : T_Var_Vectors.Vector;
      --  Formal arguments for this function, in the order they appear

      Arguments_Indexed : T_Var_Maps.Map;
      --  Formal arguments for this function, ordered by name

      Program : T_Command_Sequence;
      --  The command sequence for this function
      --  TODO: we can go one step further and implement a function with a
      --  simple command instead, to allow simpler form:
      --     function a () match bla pick bla2;
      --  without the need of a do block.
   end record;
   --  Models a user-defined function

   type String_Part_Kind is (Str_Kind, Expr_Kind, Group_Kind);
   --  Strings are composed of various parts, raw text, expressions introduced
   --  wiht \e<> and group references introduced with \number.

   type String_Kind is
     (String_Simple, String_Raw, String_Indent, String_Regexp);
   --  Models the various kinds of strings supported by the language, and
   --  controlled by the string prefix, e.g. x".*" is a regular expression
   --  string.

   type String_Part (Kind : String_Part_Kind := Str_Kind) is record
      Offset_Line, Offset_Column : Integer;
      --  Offset of that part relative to the begining of the string.
      --  TODO: Offset_Column does not currently support tabs

      Indent : Integer;
      --  If the enclosing string is an indent string, stored the requested
      --  level of indentation to add in fromnt of that part.

      case Kind is
         when Str_Kind =>
            Value : Unbounded_Text_Type;
            --  Value of the raw string

         when Expr_Kind =>
            Expr   : T_Expr;
            --  Pointer to the string expression to evaluate when computing
            --  that part

         when Group_Kind =>
            Group_Number : Integer;
            --  Capture group to retreive when computing that part

      end case;
   end record;
   --  Models a section of a string literal.

   package Processed_String_Vector is new Ada.Containers.Vectors
     (Positive, String_Part);
   use Processed_String_Vector;

   type T_Expr_Type
     (Kind : Template_Node_Kind_Type)
   is new T_Entity_Type with record
      Has_New : Boolean := False;
      --  This flag is set to true if there's an allocator new () somewhere
      --  either on this expression section or below, as certain processes
      --  forbig these kind of expressions.

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
            null;

         when Template_Number =>
            Number : Integer;

         when Template_Str =>
            Str_Kind : String_Kind;
            Str      : Processed_String_Vector.Vector;

         when Template_Call_Expr =>
            Call_Called : T_Expr;
            Call_Args   : T_Arg_Vectors.Vector;

         when Template_Defer_Expr =>
            Deferred_Expr    : T_Expr;
            Deferred_Closure : Text_Sets.Set;

         when Template_New_Expr =>
            New_Tree : T_Create_Tree;

         when Template_At_Ref =>
            null;

         when Template_Qualified_Match =>
            Qualified_Match_Expr : T_Expr;

         when Template_Fold_Expr =>
            Fold_Default   : T_Expr;
            Fold_Combine   : T_Expr;
            Fold_Separator : T_Expr;

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
            Quantifier_Min  : Integer;
            Quantifier_Max  : Integer;

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
   --  This type models a section in an expression, and pre-processes various
   --  sections.

   overriding procedure Resolve_References (An_Entity : access T_Expr_Type);
   --  Preprocesses any necessary information for this expressions and its
   --  children.

   type T_Arg_Type is new T_Entity_Type with record
      Name_Node : Identifier;
      --  If this argument is named, pointer to the node that contains that
      --  name

      Expr      : T_Expr;
      --  The expression valuating that argument
   end record;
   --  This type model an actual argument in a function or template call.

   type T_Create_Tree_Type is new T_Entity_Type with record
      Call    : T_Template_Call;
      --  Refers the the call creating the template instance for this section

      Subtree : T_Create_Tree_Vectors.Vector;
      --  List the children tree to create if any
   end record;
   --  This type models a section in a tree allocation. For example in:
   --     new (A () {B (), C ()})

end Wrapping.Semantic.Structure;
