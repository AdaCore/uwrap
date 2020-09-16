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
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers;                  use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Utils;              use Wrapping.Utils;

package Wrapping.Runtime.Structure is
   --  The purpose is to provide the data structures created live during the
   --  wrapping process (e.g. call stacks, template instances, etc.)

   type W_Object_Type;
   type W_Object is access all W_Object_Type'Class;

   function Lt_Wrapper (Left, Right : W_Object) return Boolean;
   function Eq_Wrapper (Left, Right : W_Object) return Boolean;

   package W_Object_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, W_Object);
   use W_Object_Maps;
   package W_Object_Vectors is new Ada.Containers.Vectors (Positive, W_Object);
   use W_Object_Vectors;
   package W_Object_Sets is new Ada.Containers.Ordered_Sets
     (W_Object, Lt_Wrapper, Eq_Wrapper);
   use W_Object_Sets;
   package W_Object_Any_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (W_Object, W_Object, Lt_Wrapper, Eq_Wrapper);
   use W_Object_Any_Maps;

   type Text_Buffer_Cursor;

   type Buffer_Slice;

   type Closure_Type;
   type Closure is access all Closure_Type;

   type Deferred_Command_Type;
   type Deferred_Command is access all Deferred_Command_Type;
   package Deferred_Command_Vectors is new Ada.Containers.Vectors
     (Positive, Deferred_Command);
   use Deferred_Command_Vectors;

   type W_Object_Type is tagged record
      null;
   end record;
   --  This is the root type of all values that are manipulated by expressions

   procedure Include_Symbol (Name : Text_Type; Object : not null W_Object);

   function W_Stack_Size return Natural with Inline;

   function Push_Value
     (An_Entity : access W_Object_Type; Name : Text_Type) return Boolean is
     (False) with
      Post'Class => W_Stack_Size'Old =
      (if Push_Value'Result then W_Stack_Size - 1 else W_Stack_Size);

   procedure Push_Call_Result
     (An_Entity : access W_Object_Type; Params : T_Arg_Vectors.Vector)
     with Post'Class => W_Stack_Size = W_Stack_Size'Old + 1;
   --  Calling an entity means either doing an actual call if this entity
   --  refers to a function, or performing a comparison between the object
   --  and the provided parameters. In the second case, by convention, the
   --  result of the call (last object on the stack) is either Match_False,
   --  or Match_True (An_Entity). By default, this returns an error (the
   --  object is not made for being called).

   function Is_Generator (An_Entity : access W_Object_Type) return Boolean
   is (False);
   --  If this returns true, then the Push_Call_Result call before is
   --  responsible to call the yield callback, if any. Otherwise, this is done
   --  by the caller.

   procedure Generate_Values (Object : access W_Object_Type; Expr : T_Expr);
   --  If this object is a container or a generator, will generate values
   --  matching the expression given in parameter one by one, calling the
   --  yield action. If no Yield action is set in the frame, only generate
   --  the first matching one, false if none.

   function Match_With_Top_Object
     (An_Entity : access W_Object_Type) return Boolean;
   --  Match this object with the top of the stack. Return False if no decision
   --  could be made, true otherwise. If the top object doesn't match, replace
   --  it with a match false.

   type Browse_Mode is
     (Parent, Child_Depth, Child_Breadth, Next, Prev, Sibling, Wrapper);

   --  TODO: This is probably actually needed only at the node level.
   function Traverse
     (An_Entity  : access W_Object_Type; A_Mode : Browse_Mode;
      Include_It : Boolean; Final_Result : out W_Object;
      Visitor    : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action)
      return Visit_Action;

   --  TODO this into push browse_Result? And see with the other push browse
   --  result what should be changed
   procedure Evaluate_Bowse_Functions
     (An_Entity        : access W_Object_Type; A_Mode : Browse_Mode;
      Match_Expression : T_Expr) is null;

   pragma Warnings (Off, "postcondition does not mention");
   function Browse_Entity
     (Browsed : access W_Object_Type'Class;
      Match_Expression : T_Expr;
      Result  : out W_Object) return Visit_Action with
     Post => W_Stack_Size = W_Stack_Size'Old;
   pragma Warnings (On, "postcondition does not mention");

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

   function Dereference (Object : access W_Object_Type) return W_Object is
     (W_Object (Object));
   --  If Object represents a reference, returns the referenced object
   --  (recursively) otherwise self.

   function Lt
     (Left : access W_Object_Type; Right : access W_Object_Type'Class)
      return Boolean;

   function Eq
     (Left : access W_Object_Type; Right : access W_Object_Type'Class)
      return Boolean;

   Null_Object : constant W_Object := new W_Object_Type;

   function Get_Object_For_Entity
     (An_Entity : access T_Entity_Type'Class) return W_Object;

   type Closure_Type is record
      Captured_Symbols : W_Object_Maps.Map;
      Implicit_It      : W_Object;
      Lexical_Scope    : T_Entity;
      Temp_Names       : Text_Maps_Access;
      Left_Value       : W_Object;
   end record;

   type Deferred_Command_Type is record
      Command   : T_Command;
      A_Closure : Closure;
   end record;

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
