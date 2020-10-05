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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Containers;                  use Ada.Containers;

with Wrapping.Utils;              use Wrapping.Utils;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Strings;    use Wrapping.Runtime.Strings;
with Wrapping.Runtime.Objects;    use Wrapping.Runtime.Objects;

package Wrapping.Runtime.Nodes is

   type W_Node_Type;
   type W_Node is access all W_Node_Type'Class;
   package W_Node_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, W_Node);
   use W_Node_Maps;
   package W_Node_Vectors is new Ada.Containers.Vectors (Positive, W_Node);
   use W_Node_Vectors;

   type W_Template_Instance_Type;
   type W_Template_Instance is access all W_Template_Instance_Type'Class;
   package W_Template_Instance_Maps is new Ada.Containers
     .Indefinite_Ordered_Maps
       (Text_Type, W_Template_Instance);
   use W_Template_Instance_Maps;
   package W_Template_Instance_Vectors is new Ada.Containers.Vectors
     (Positive, W_Template_Instance);
   use W_Template_Instance_Vectors;

   type W_Hollow_Node_Type;
   type W_Hollow_Node is access all W_Hollow_Node_Type'Class;

   type W_Node_Type is new W_Object_Type with record
      Parent, Next, Prev : W_Node;
      --  References the parent, prev and next node in the tree where this node
      --  belongs to

      Children_Ordered : W_Node_Vectors.Vector;
      --  Children of the current node in the order they were created.

      Children_Indexed : W_Node_Maps.Map;
      --  Same as before, but indexed by name

      Wrappers_By_Name    : W_Template_Instance_Maps.Map;
      --  Register wrappers by they short name (without module prefix).
      --  TODO: this creates sitations where an entity can't be wrapped by
      --  two different wrappers of the same name in two different modules.
      --  While in practice this is a rare and non-advisable behavior, we
      --  may need to clarify the semantic and allow it in specific
      --  circumstances.

      Wrappers_By_Full_Id : W_Template_Instance_Maps.Map;
      --  Same as before, but wrappers are registered by they full id, with all
      --  modules as prefix.

      Wrappers_Ordered    : W_Template_Instance_Vectors.Vector;
      --  Same as before, but wrappers are registered in the order they were
      --  created

      Forbidden_Wrapper_Types : Text_Sets.Set;
      --  List of wrapper types (template) listed by they full id that can't
      --  be wrapping this entity. Typically wrapped by a null clause, e.g.:
      --     wrap null (some_name);

      Tmp_Counter : Integer := 0;
      --  When creating temporary names, this counter is incremented when
      --  needed and used in the actual temporary id to ensure uniqueness.
   end record;
   --  W_Nodes are at the core of the processing. They compose tree structures
   --  that can map either an input tree or instantiated objects, and are
   --  derived in various types that implement specific capabilites. In
   --  particular, input languages have all their own derivation of node.

   function Language (An_Entity : W_Node_Type) return Text_Type is ("");
   --  Provides a name for the language that this node represents. Retreived
   --  from the language intrinsic field.

   overriding function Type_Name
     (Object : W_Node_Type) return Text_Type is
     ("node (" & W_Node_Type'Class (Object).Language & ")");
   --  See parent documentation

   procedure Add_Child (Parent, Child : access W_Node_Type'Class);
   --  Connects Parent and Child with a "child" relationship. Child is
   --  nameless.

   procedure Add_Child
     (Parent, Child : access W_Node_Type'Class; Name : Text_Type);
   --  Connects Parent and Child with a "child" relationship and register
   --  this child with a given name

   procedure Add_Next (Cur, Next : access W_Node_Type'Class);
   --  Connects Cur and Next with a "next" relationship. If Cur has a parent,
   --  then the parent of Cur will also be the parent of Next.

   procedure Add_Child_With_Wrapping
     (Parent, Child : access W_Node_Type'Class);
   --  Similar to Add_Child, but if the Parent is a wrapping entity, will
   --  create hollow nodes as a child to the origin of the parent and set Child
   --  as a wrapper of this node instead of creating a direct link between
   --  parent and Child.

   function Get_Wrapper
     (An_Entity : access W_Node_Type'Class; Name : Text_Type)
      return W_Template_Instance;
   --  If An_Entity is wrapped by a wrapper of the given short name, returns
   --  the instance, otherwise null.

   function Get_Wrapper
     (An_Entity : access W_Node_Type'Class; A_Template : T_Template)
      return W_Template_Instance;
   --  If An_Entity is wrapped by a wrapper of the given type, returns
   --  the instance, otherwise null.

   overriding function Push_Value
     (An_Entity : access W_Node_Type; Name : Text_Type) return Boolean;
   --  Pushes intrinsic functions if they correspond to the name.

   overriding procedure Push_Call_Result
     (An_Entity : access W_Node_Type; Params : T_Arg_Vectors.Vector);
   --  Pushes the result of the match between the entity and the first
   --  expression.

   overriding function Match_With_Top_Object
     (An_Entity : access W_Node_Type) return Boolean;
   --  Matches the node with the top object.

   procedure Pre_Visit (An_Entity : access W_Node_Type) is null;
   --  Some nodes may not compute all of their data upon creation, for example
   --  children may be computed lazyly. This call is made prior to visiting
   --  a node structure, for example a traversal, so that nodes that make lazy
   --  computations can compute what's needed.

   overriding function Traverse
     (An_Entity    : access W_Node_Type;
      A_Mode       : Traverse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
      return Visit_Action)
      return Visit_Action;
   --  Traverse the current node. See parent for full documentation

   overriding procedure Push_Traverse_Result
     (An_Entity        : access W_Node_Type;
      A_Mode           : Traverse_Mode;
      Match_Expression : T_Expr);
   --  Push the traverse result to the stack, and perform operations related
   --  to dynamic allocation of nodes outside of wrapping:
   --    * if the initial traverse didn't find a match, it may be the case
   --      that we're on a boolean expression that accepts or require an
   --      allocation as a side effect, e.g.:
   --         match a or new (some_type ())
   --      in that case, the Match_Expression will be evaluated first without
   --      allowing allocations, then a second time with allocations enabled.
   --   * on certain modes, allocation do create links between the node under
   --     traveral and the new node, e.g.:
   --        child (new (some_type ()))
   --     will create a child link between the new object and the current one.
   --     this call will take care of creating that link.
   --   * When allocating new nodes on a tree wrapping another tree, will
   --     create hollow nodes to the original tree to link with a wrapping mode
   --     to the current one.

   overriding
   function Write_String
     (An_Entity : W_Node_Type) return Buffer_Slice is (Get_Empty_Slice);
   --  See parent documentation.

   type W_Template_Instance_Type is new W_Node_Type with record
      Defining_Entity : T_Entity;
      --  References the entity from which this template has been instantiated
      --  from. It can be either a template or the implicit template object
      --  created for modules.

      Indexed_Variables : W_Reference_Maps.Map;
      --  This is used to record the actual values for the template variables.
      --  There is always one level of indirection between the variable and its
      --  actual data, so that when the variable is modified, references to it
      --  are updated accordingly. For example, if you have something like:
      --     weave with (X => "somethign", Y => X);
      --  the reference to X is indirect, and if X changes through another
      --  weaver, it will also change Y. The only time where the symbol
      --  is automatically dereferenced is when using it as a left value
      --  reference, such as:
      --      weave with (X => @ & "somethign");
      --  which would otherwise create cycles and defeat the intended semantic.

      Ordered_Variables : W_Reference_Vectors.Vector;
      --  Same as Index_Variables, but recorded in the order of appearance
      --  instead of by name

      Origin : W_Node;
      --  If this template instance wraps a node, points to the wrapped node

      Is_Wrapping : Boolean := False;
      --  A template can only be wrapped once on a node (but weaved multiple
      --  times). This flags turns to true when that instance is used in a
      --  wrap context the first time.

      Is_Evaluated : Boolean := False;
      --  This is true after the first time the template has been evaluated.
   end record;
   --  Template instances are a specific kind of node that represent the
   --  instantiation of a user defined template. It could be created either
   --  through a wrap / weave clause, or a new () allocator.

   overriding function Type_Name
     (Object : W_Template_Instance_Type)
      return Text_Type is ("template instance");
   --  See parent documentation

   function Create_Template_Instance
     (A_Template : T_Template;
      Wrapping   : access W_Node_Type'Class;
      Register   : Boolean) return W_Template_Instance;
   --  Create a template instance after the template type in parameter. If
   --  Wrapping is not null, the template instance will be linked to the
   --  wrapping node through a wrapping / origin relationship. If register
   --  is true, the new template instance will also be added to the various
   --  templates lists, in particular the deferred analyis and the list
   --  of templates for this template type.

   overriding function Push_Value
     (An_Entity : access W_Template_Instance_Type; Name : Text_Type)
      return Boolean;
   --  Pushes intrinsics or specific variables values for this template
   --  instance.

   overriding function Match_With_Top_Object
     (An_Entity : access W_Template_Instance_Type) return Boolean;
   --  Matches the top object with the current template - in particular if the
   --  top object is a static reference to a template type.

   overriding function Traverse
     (An_Entity  : access W_Template_Instance_Type; A_Mode : Traverse_Mode;
      Include_Self : Boolean; Final_Result : out W_Object;
      Visitor    : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
      return Visit_Action)
      return Visit_Action;
   --  When created outside of a wrapping relationship, template instances
   --  iteration behaves the same as other nodes. However, when they are
   --  wrapping another node (ie they have an origin), the wrapping
   --  relationship between wrapped nodes are derived for those of their
   --  origin. So for example, if A is parent of B, and if w_A1 and w_A2 wrap
   --  A, w_B1 and w_B2 wraps B, w_A1 and w_A2 are both parents of w_B1 and
   --  w_B2.

   overriding function Language
     (An_Entity : W_Template_Instance_Type) return Text_Type is
     ("template");
   --  See parent documentation

   type W_Hollow_Node_Type is new W_Template_Instance_Type with record
      null;
   end record;
   --  This type of node is created when instantiated node on wrappers. For
   --  example, when writing:
   --
   --  template A do end;
   --  template B do end;
   --
   --  match Entity ()
   --  do
   --     wrap x: A ()
   --  then
   --     pick x.child (new (B ()));
   --  end;
   --
   --  A and B are in the wrapper tree. There is no real parent / child
   --  relationship between A and B. Instead, A is wrapping the entity, new
   --  B is actually creating a hollow node under the current entity, wrapped
   --  with B. This structure is necessary to enable browsing of the wrapping
   --  tree, which is based on browsing the original input tree.

   overriding function Type_Name
     (Object : W_Hollow_Node_Type)
      return Text_Type is ("hollow node");
   --  See parent documentation

   overriding function Language
     (An_Entity : W_Hollow_Node_Type) return Text_Type is ("hollow");
   --  See parent documentation

end Wrapping.Runtime.Nodes;
