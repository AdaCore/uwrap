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

--  This package declares the types of the objects manipulated by the wrapping
--  language expression, all derived from W_Object_Type. They are all prefixed
--  by W_ to be able to easily distinguish them from other types.

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers;                  use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Utils;              use Wrapping.Utils;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Strings;    use Wrapping.Runtime.Strings;
with Wrapping.Runtime.Matching;   use Wrapping.Runtime.Matching;

package Wrapping.Runtime.Objects is

   type W_Reference_Type;
   type W_Reference is access all W_Reference_Type;
   package W_Reference_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, W_Reference);
   use W_Reference_Maps;
   package W_Reference_Vectors is new Ada.Containers.Vectors
     (Positive, W_Reference);
   use W_Reference_Vectors;

   type W_Vector_Type;
   type W_Vector is access all W_Vector_Type'Class;
   package W_Vector_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, W_Vector);
   use W_Vector_Maps;

   type W_Set_Type;
   type W_Set is access all W_Set_Type;

   type W_Map_Type;
   type W_Map is access all W_Map_Type;

   type W_Integer_Type;
   type W_Integer is access all W_Integer_Type'Class;

   type W_Text_Expression_Type;
   type W_Text_Expression is access all W_Text_Expression_Type'Class;
   package W_Text_Expression_Vectors is new Ada.Containers.Vectors
     (Positive, W_Text_Expression);
   use W_Text_Expression_Vectors;

   type W_String_Type;
   type W_String is access all W_String_Type'Class;

   type W_Regexp_Type;
   type W_Regexp is access all W_Regexp_Type'Class;

   type W_Text_Conversion_Type;
   type W_Text_Conversion is access all W_Text_Conversion_Type'Class;

   type W_Intrinsic_Function_Type;
   type W_Intrinsic_Function is access all W_Intrinsic_Function_Type'Class;

   type W_Function_Type;
   type W_Function is access all W_Function_Type'Class;

   type W_Static_Entity_Type;
   type W_Static_Entity is access all W_Static_Entity_Type'Class;

   type W_Deferred_Expr_Type;
   type W_Deferred_Expr is access all W_Deferred_Expr_Type'Class;

   type W_Regexpr_Result_Type;
   type W_Regexpr_Result is access all W_Regexpr_Result_Type'Class;

   type W_Reference_Type is new W_Object_Type with record
      Value : W_Object;
      --  The object pointed by this reference

      Is_Implicit_It : Boolean := False;
      --  Is this reference an implicit iterator?

      Is_Allocated : Boolean := False;
      --  When entities are created in the expressions through the new
      --  function, this flag is set to true, so that the entity can be
      --  capture by enclosing functions such as child or sibling.
   end record;
   --  This record is used to provide an indirection to a W_Object, and
   --  possibly decorate this indirection with specific flags. All primitive
   --  are pass through.

   overriding function Push_Value
     (An_Entity : access W_Reference_Type; Name : Text_Type) return Boolean is
     (An_Entity.Value.Push_Value (Name));
   --  Pass through to the Value primitive.

   overriding procedure Push_Call_Result
     (An_Entity : access W_Reference_Type; Params : T_Arg_Vectors.Vector);
   --  Pass through to the Value primitive.

   overriding function Is_Generator
     (An_Entity : access W_Reference_Type) return Boolean
   is (An_Entity.Dereference.Is_Generator);
   --  Pass through to the Value primitive.

   function Match_With_Top_Object
     (An_Entity : access W_Reference_Type) return Boolean;
   --  Pass through to the Value primitive.

   overriding function Traverse
     (An_Entity    : access W_Reference_Type; A_Mode : Traverse_Mode;
      Include_Self : Boolean; Final_Result : out W_Object;
      Visitor      : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action)
      return Visit_Action;
   --  Pass through to the Value primitive.

   overriding procedure Push_Traverse_Result
     (An_Entity        : access W_Reference_Type; A_Mode : Traverse_Mode;
      Match_Expression : T_Expr);
   --  Pass through to the Value primitive.

   function Is_Implicit (Object : W_Reference_Type) return Boolean is
     (Object.Is_Implicit_It);
   --  Pass through to the Value primitive.

   overriding function Dereference
     (Object : access W_Reference_Type) return W_Object is
     (Object.Value.Dereference);
   --  Pass through to the Value primitive.

   overriding function Write_String
     (Object : W_Reference_Type) return Buffer_Slice is
     (if Object.Value /= null
      then Object.Value.Write_String
      else Get_Empty_Slice);
   --  Pass through to the Value primitive.

   overriding function To_Debug_String
     (Object : W_Reference_Type) return Text_Type is
     (if Object.Value /= null then Object.Value.To_Debug_String else "");
   --  Pass through to the Value primitive.

   procedure Generate_Values (Object : access W_Reference_Type; Expr : T_Expr);
   --  Pass through to the Value primitive.

   type W_Vector_Type is new W_Object_Type with record
      A_Vector : W_Object_Vectors.Vector;
   end record;
   --  Holds a vector of runtime objects.

   function Is_Text_Container (Container : W_Vector_Type) return Boolean;
   --  Checks that this container only contains derivatives of
   --  Runtime_Text_Expression_Type.

   overriding function Push_Value
     (An_Entity : access W_Vector_Type; Name : Text_Type) return Boolean;
   --  Pushes intrinsic functions for vector, or parent entities

   overriding function Write_String
     (Object : W_Vector_Type) return Buffer_Slice;
   --  Concatenates the string value of all objects contained in the vector

   overriding procedure Generate_Values
     (Object : access W_Vector_Type; Expr : T_Expr);
   --  Generates values for all objects contained in the vector that match the
   --  expression given in parameter

   type W_Set_Type is new W_Object_Type with record
      A_Set : W_Object_Sets.Set;
   end record;
   --  Holds a set of runtime objects.

   overriding function Push_Value
     (An_Entity : access W_Set_Type; Name : Text_Type) return Boolean;
   --  Pushes intrinsic functions for set, or parent entities

   overriding procedure Generate_Values
     (Object : access W_Set_Type; Expr : T_Expr);
   --  Generates values for all objects contained in the set that match the
   --  expression given in parameter

   type W_Map_Type is new W_Object_Type with record
      A_Map : W_Object_Any_Maps.Map;
   end record;
   --  Holds a set of runtime objects.

   overriding function Push_Value
     (An_Entity : access W_Map_Type; Name : Text_Type) return Boolean;
   --  Pushes intrinsic functions for map, or parent entities

   overriding procedure Generate_Values
     (Object : access W_Map_Type; Expr : T_Expr);
   --  Generates values for all objects contained in the map that match the
   --  expression given in parameter

   type W_Integer_Type is new W_Object_Type with record
      Value : Integer;
   end record;
   --  Holds an integer string

   overriding function Write_String
     (Object : W_Integer_Type) return Buffer_Slice;
   --  Converts integer to string

   type W_Text_Expression_Type is abstract new W_Object_Type with record
      null;
   end record;
   --  Root type to types that are variants of string objects

   overriding procedure Push_Call_Result
     (An_Entity : access W_Text_Expression_Type;
      Params    : T_Arg_Vectors.Vector);
   --  Matches the object against the first parameter

   type W_String_Type is new W_Text_Expression_Type with record
      Value : Unbounded_Text_Type;
   end record;
   --  Models a simple string

   overriding function Write_String
     (Object : W_String_Type) return Buffer_Slice;
   --  Writes the contents on the string on the buffer

   overriding function Lt
     (Left : access W_String_Type; Right : access W_Object_Type'Class)
      return Boolean;
   --  Performs character by character lower than comparison

   overriding function Eq
     (Left : access W_String_Type; Right : access W_Object_Type'Class)
      return Boolean;
   --  Performs character by character equality

   function To_W_String (Str : Text_Type) return W_String;
   --  Transforms a Text_Type to a newly allocated W_String

   function To_W_String (Str : Unbounded_Text_Type) return W_String;
   --  Transforms a Unbounded_Text_Type to a newly allocated W_String

   type W_Regexp_Type is new W_Text_Expression_Type with record
      Value : Unbounded_Text_Type;
   end record;
   --  Holds a regular expression

   overriding function Write_String
     (Object : W_Regexp_Type) return Buffer_Slice;
   --  Writes the content of the regular expression in the buffer

   type W_Text_Conversion_Type is new W_Text_Expression_Type with record
      An_Object : W_Object;
   end record;
   --  This type is used to model an object that needs to converts a sub-object
   --  into string. This is useful to differenciate sitations where a piece of
   --  data has to be interpreted as a reference or as a text.

   overriding function Write_String
     (Object : W_Text_Conversion_Type) return Buffer_Slice;
   --  Calls Write_String on the contained object

   type Call_Access is access procedure
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);
   --  This type references an instrinsinc call between an object and its
   --  parameters

   type W_Intrinsic_Function_Type is new W_Object_Type with record
      Prefix    : W_Object;
      --  The object on which the intrinsic function will be call

      Call      : Call_Access;
      --  Callback to call

      Generator : Boolean := False;
      --  Wether this function is a generator (responsible to call yield
      --  callbacks) or a single value function (which will not call yield).
   end record;
   --  Models an intrinsic call and the object to call it on

   overriding procedure Push_Call_Result
     (An_Entity : access W_Intrinsic_Function_Type;
      Params    : T_Arg_Vectors.Vector);
   --  Pushes a frame and calls the callback on the prefix previously stored

   overriding function Is_Generator
     (An_Entity : access W_Intrinsic_Function_Type) return Boolean
   is (An_Entity.Generator);
   --  See parent documentation

   procedure Push_Intrinsic_Function (Prefix : W_Object; A_Call : Call_Access);
   --  Instantiate a new W_Instrinsic_Function object for the prefix and the
   --  call in parameter, push that to the stack.

   type W_Function_Type is new W_Object_Type with record
      A_Function : T_Function;
   end record;
   --  Models a user-defined function

   overriding procedure Push_Call_Result
     (An_Entity : access W_Function_Type; Params : T_Arg_Vectors.Vector);
   --  Pushes a frame and evaluate the function referenced by this object on
   --  the given parameter.

   overriding function Is_Generator
     (An_Entity : access W_Function_Type) return Boolean
   is (True);
   --  All user functions are generators. They will call yield on the picked
   --  values.

   type W_Static_Entity_Type is new W_Object_Type with record
      An_Entity : T_Entity;
   end record;
   --  References a static entity, for example a module prefix in a module
   --  expression.

   overriding function Push_Value
     (An_Entity : access W_Static_Entity_Type; Name : Text_Type)
      return Boolean;
   --  If this semantic entity is a scope, e.g. a module, pushes the child that
   --  correspinds to the name in parameter

   overriding procedure Push_Call_Result
     (An_Entity : access W_Static_Entity_Type; Params : T_Arg_Vectors.Vector);
   --  Checks the static entity against the first parameter. If successful,
   --  pushes the implicit it

   overriding procedure Generate_Values
     (Object : access W_Static_Entity_Type; Expr : T_Expr);
   --  Generates values for each template of the given static expression type
   --  that has been created, none if the static expression is not a template.

   type W_Deferred_Expr_Type is new W_Object_Type with record
      A_Closure : Closure;
      Expr      : T_Expr;
   end record;
   --  A defered expression is an expression that will be evaluated as late
   --  as possible, during the Write_String calls. The environment is captured
   --  at creation time to that the expression can be valuated later on.

   overriding function Write_String
     (Object : W_Deferred_Expr_Type) return Buffer_Slice;
   --  Restores the closure in a new frame, evaluates the stored expression and
   --  converts it to string.

   procedure Capture_Deferred_Environment
     (Deferred_Expr : W_Deferred_Expr; Expr : T_Expr);
   --  Captures the deferred environment related to this expression.

   type W_Regexpr_Result_Type is new W_Object_Type with record
      Result : W_Vector;
   end record;
   --  Models the result of a regular expression, which is a series of nodes
   --  that were captured by this expression. By default, all primitives
   --  of this types behave as if it were a reference to the last element,
   --  except for generation that generate values for all elements.

   overriding procedure Generate_Values
     (Object : access W_Regexpr_Result_Type; Expr : T_Expr);
   --  Generate values for all elements in the regexpr

   function As_Singleton
     (Object : W_Regexpr_Result_Type'Class) return W_Object
   is
     (if Object.Result.A_Vector.Length >= 1 then
        Object.Result.A_Vector.Last_Element
      else Match_False);
   --  Returns the value of the regexpr as if it were a singleton, which is
   --  processing its last element.

   function Push_Value
     (An_Entity : access W_Regexpr_Result_Type; Name : Text_Type)
      return Boolean is
     (An_Entity.As_Singleton.Push_Value (Name));
   --  Calls Push_Value on the last element of the result.

   overriding procedure Push_Call_Result
     (An_Entity : access W_Regexpr_Result_Type; Params : T_Arg_Vectors.Vector);
   --  Calls Push_Call_Result on the last element of the result.

   overriding function Is_Generator
     (An_Entity : access W_Regexpr_Result_Type) return Boolean
   is (An_Entity.As_Singleton.Is_Generator);
   --  Calls Is_Generator on the last element of the result.

   function Match_With_Top_Object
     (An_Entity : access W_Regexpr_Result_Type) return Boolean is
     (An_Entity.As_Singleton.Match_With_Top_Object);
   --  Calls Match_With_Top_Object on the last element of the result.

   overriding function Traverse
     (An_Entity  : access W_Regexpr_Result_Type; A_Mode : Traverse_Mode;
      Include_It : Boolean; Final_Result : out W_Object;
      Visitor    : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action)
      return Visit_Action;
   --  Calls Traverse on the last element of the result.

   overriding procedure Push_Traverse_Result
     (An_Entity        : access W_Regexpr_Result_Type; A_Mode : Traverse_Mode;
      Match_Expression : T_Expr);
   --  Calls Push_Traverse_Result on the last element of the result.

   overriding function Write_String
     (Object : W_Regexpr_Result_Type) return Buffer_Slice is
     (Object.As_Singleton.Write_String);
   --  Calls Write_String on the last element of the result.\

end Wrapping.Runtime.Objects;
