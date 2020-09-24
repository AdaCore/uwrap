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

--  This package provides data structures required by most of the runtime
--  processing.

with Ada.Containers;                  use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Utils;              use Wrapping.Utils;

package Wrapping.Runtime.Structure is

   type W_Object_Type;
   type W_Object is access all W_Object_Type'Class;

   function Lt_Wrapper (Left, Right : W_Object) return Boolean;
   --  Wraps the Lt primitives of W_Object_Type in a way that can then be used
   --  in sets of W_Object

   function Eq_Wrapper (Left, Right : W_Object) return Boolean;
   --  Wraps the Eq primitives of W_Object_Type in a way that can then be used
   --  in sets of W_Object

   package W_Object_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, W_Object);
   use W_Object_Maps;
   package W_Object_Vectors is new Ada.Containers.Vectors (Positive, W_Object);
   use W_Object_Vectors;
   package W_Object_Sets is new Ada.Containers.Ordered_Sets
     (W_Object, Lt_Wrapper, Eq_Wrapper);
   package W_Object_Any_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (W_Object, W_Object, Lt_Wrapper, Eq_Wrapper);

   type Text_Buffer_Cursor;

   type Buffer_Slice;

   type Closure_Type;
   type Closure is access all Closure_Type;

   type W_Object_Type is tagged record
      null;
   end record;
   --  This is the root type of all values that are manipulated by expressions

   procedure Include_Symbol (Name : Text_Type; Object : not null W_Object);
   --  Adds or replace a symbol of a given name in the current frame.

   function W_Stack_Size return Natural with Inline;
   --  Returns the current stack of W_Object in the current frame.

   function Push_Value
     (An_Entity : access W_Object_Type; Name : Text_Type) return Boolean with
      Post'Class => W_Stack_Size'Old =
       (if Push_Value'Result then W_Stack_Size - 1 else W_Stack_Size);
   --  Looks in the current object members for a given name. If found, the
   --  corresponding object will be pushed to the stack, and this function
   --  will return True, otherwise nothing is pushed on the stack and this
   --  function returns false.

   procedure Push_Call_Result
     (An_Entity : access W_Object_Type; Params : T_Arg_Vectors.Vector) with
     Post'Class => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Some W_Objects are callable, that is they can take a set of parameters
   --  e.g.:
   --    it (<expression>)
   --  will run a match of the expression against it, or
   --    to_lower (<expression>)
   --  will call the to_lower function.
   --  This function will raise an exeption if the entity is actually not
   --  callable.
   --  TODO: Is that a friendly behavior in the context of a match? Shouldn't
   --  we stack false instead?

   function Is_Generator (An_Entity : access W_Object_Type) return Boolean
   is (False);
   --  If this returns true, then the Push_Call_Result call before is
   --  a generator responsible to call the yield callback, if any. Otherwise,
   --- this is done by the caller.

   procedure Generate_Values (Object : access W_Object_Type; Expr : T_Expr)
     with Post'Class => W_Stack_Size = W_Stack_Size'Old + 1;
   --  If this object is a container or a generator, will generate values
   --  matching the expression given in parameter one by one, calling the
   --  yield action. If no Yield action is set in the frame, only generate
   --  the first matching one, false if none. Note that this is different from
   --  Push_Call_Result, which may also generate values from a call, e.g.:
   --    x ().all ()
   --  This generates values directly on the object, e.g.:
   --    x.all ()

   function Match_With_Top_Object
     (An_Entity : access W_Object_Type) return Boolean
     with Post'Class => W_Stack_Size = W_Stack_Size'Old;
   --  Match this object with the top of the stack. Return False if no decision
   --  could be made, true otherwise. If the top object doesn't match, replace
   --  it with a match false.

   type Traverse_Mode is
     (Parent, Child_Depth, Child_Breadth, Next, Prev, Sibling, Wrapper);
   --  Traveral is provided for various kind of modes - this type identifies
   --  which specific one should be used.

   function Traverse
     (An_Entity    : access W_Object_Type;
      --  The object at the root of the traversed relation.

      A_Mode       : Traverse_Mode;
      --  Defines the mode in which the traveral is expected to happen.

      Include_Self : Boolean;
      --  When true, the value of An_Entity will be the first one on which the
      --  visitor is called, false otherwise.

      Final_Result : out W_Object;
      --  Provides the final non-null result of the visitor calls.

      Visitor      : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action
      --  Called on each visited node. As a result of the visit, the visitor
      --  may set a specific value as the result of its operation, to be
      --  transfered later by the overall Traverse function. This function
      --  can also provide a specific decision as to how to carry on the visit,
      --  which can be Over (to avoid e.g. looking at children in the child
      --  mode) or Stop to interrupt the traversal.
     )
      return Visit_Action
     with Post'Class => W_Stack_Size = W_Stack_Size'Old;
   --  Traverse entities related to the entity in parameter in the mode
   --  provided in parameter. If the outcome of the traveral is a specific
   --  visit action to be passed to the calling context, it'll be returned,
   --  Unknown otherwise. Note that Traverse can be called by various
   --  processes, including but not limited to expressions. As a result it
   --  returns its resulting value as opposed to pushing it on the stack like
   --  many others.

   procedure Push_Traverse_Result
     (An_Entity        : access W_Object_Type;
      A_Mode           : Traverse_Mode;
      Match_Expression : T_Expr)
     with Post'Class => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Wrapper around Traverse for the purpose of evalating a traverse function
   --  in the context of an expression. Will also be responsible of expression
   --  traversal post processing such as linkage with new nodes or hollow
   --  nodes creation (see documentation in the W_Node overriden function for
   --  more details).

   function Process_Generated_Value
     (Generated        : access W_Object_Type'Class;
      Match_Expression : T_Expr) return Visit_Action with
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Process an entity that has been generated a new entity. If
   --  Match_Expession is not null, then the entity will be matched against the
   --  match expression and will only be further processed if the match is
   --  positive, result will be null otherwise. If this function is called in
   --  the context of value generation, then the Yield callback will be called,
   --  and Result will be set to the result of that callback. Otherwise, Result
   --  will take the value of Generated.
   --  This function will also take care of setting the value for the capturing
   --  variable, so that in expressions like:
   --     x: child (<some expression>)
   --  x can be accessible in <some expression>.

   function Write_String (Object : W_Object_Type) return Buffer_Slice;
   --  This function resolves a runtime object into the String value, and
   --  writes this string in the global string buffer. The result of this
   --  function varies over time - as the underlying object gets completed by
   --  various wrapping and weaving opertions. This should be called as late as
   --  possible in the process (unless explicitely requested through e.g. a
   --  string conversion). Keeping a reference to the runtime object is
   --  prefered. Note that this is directly linked to the actual semantics of
   --  the language, so should remain consistent with it.

   function To_Debug_String (Object : W_Object_Type) return Text_Type
   is ("<empty>");
   --  Returns a string used for the purposed of debugging possibly based on
   --  the result of Write_String.

   function Dereference (Object : access W_Object_Type) return W_Object is
     (W_Object (Object));
   --  If Object represents a reference, returns the referenced object
   --  (recursively) otherwise self.

   function Lt
     (Left : access W_Object_Type; Right : access W_Object_Type'Class)
      return Boolean;
   --  Returns true if Left is lower than right, false otherwise. By default,
   --  does an address comparison.

   function Eq
     (Left : access W_Object_Type; Right : access W_Object_Type'Class)
      return Boolean;
   --  Returns true if Left is equal to right, false otherwise. By default,
   --  does an address comparison.

   function Get_Object_For_Entity
     (An_Entity : access T_Entity_Type'Class) return W_Object;
   --  Static entities types such as modules and template are associated with
   --  a global W_Object that allows to store related data. For example,
   --  templates store the list of all instances created after their type.
   --  This function returns the runtime object that corresponds to this
   --  static object.

   type Closure_Type is record
      Captured_Symbols : W_Object_Maps.Map;
      --  The symbols captured by this closure.

      Implicit_It      : W_Object;
      --  The value of it at the point of capture to this closure.

      Lexical_Scope    : T_Entity;
      --  A reference to the lexical scope active at the point of capture.

      Temp_Names       : Text_Maps_Access;
      --  The list of temporary names generated at the point of capture.

      Left_Value       : W_Object;
      --  The left value @, if any, active at the point of capture.
   end record;
   --  This type contains the objects necessary to capture a closure.

   type Text_Buffer_Cursor is record
      Offset      : Natural;
      --  Offset refering to position in Buffer.Str

      Line        : Natural;
      --  Current line where this cursor is located. This is only updated
      --  if Buffer.Full_Cursor_Update is true.

      Line_Offset : Natural;
      --  Current line character offset where this cursor is located. This is
      --  only updated if Buffer.Full_Cursor_Update is true.

      Column      : Natural;
      --  Current column where this cursor is located. This is only updated if
      --  Buffer.Full_Cursor_Update is true.
      --  TODO: tabs are not currently supported.

      Max_Column  : Natural;
      --  Max column that has been encountered so far in this buffer position.
   end record;
   --  Used to point to a position in the text buffer.

   type Buffer_Slice is record
      First, Last : Text_Buffer_Cursor;
   end record;
   --  Represents a slice of data in the main text buffer, First and Last are
   --  inclusive. Empty slice have a last cursor before the first cursor.

end Wrapping.Runtime.Structure;
