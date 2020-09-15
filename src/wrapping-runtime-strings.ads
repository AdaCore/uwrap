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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with Libtemplatelang.Common; use Libtemplatelang.Common;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;

package Wrapping.Runtime.Strings is

   package Text_Buffer_Cursor_Vectors is new Ada.Containers.Vectors
     (Positive, Text_Buffer_Cursor);
   use Text_Buffer_Cursor_Vectors;

   type Text_Buffer_Type is record
      Str    : Text_Access;
      --  String buffer. This is initialized at a fixed size early in the
      --  driver. Relevant data is between 1 and Cursor.Offset.

      Cursor : Text_Buffer_Cursor :=
        (Offset => 1,
         Line        => 1,
         Line_Offset => 1,
         Column      => 1,
         Max_Column  => 1);
      --  Current cursor designating the position after the last written
      --  character.

      Cursor_Stack : Text_Buffer_Cursor_Vectors.Vector;
      --  Holds the stack of cursors. Should only be accessed through
      --  Push_Buffer_Cursor and Pop_Buffer_Cursor

      Full_Cursor_Update : Boolean := False;
      --  When this is true, Write_String will update
      --  Line / Line_Offset / Column values.
   end record;
   --  This types is used to gather all data necessary to hold a global text
   --  buffer. Strings are added to this buffer during e.g. concatenation,
   --  or creation of string objects. It also provides support for counting
   --  columns and lines as text is being added, allowing to write conditions
   --  based on the current position in the text being written.

   Buffer : Text_Buffer_Type;
   --  This is the global buffer holding temporary text as it's being written
   --  by various expresions.

   procedure Evaluate_String
     (Expr : T_Expr;
      On_Group : access procedure (Index : Integer; Value : W_Object) := null;
      On_Expression : access procedure (Expr : T_Expr) := null) with
       Post => Top_Frame.Data_Stack.Length =
         Top_Frame.Data_Stack.Length'Old + 1
         and Expr.Kind = Template_Str;
   --  This procedure is the entry point to string evaluation. It will evaluate
   --  various parts of the strings, process indentation, run expression
   --  evaluation, etc. The result is a W_String value that contains the
   --  result of the evaluation. If On_Group or On_Expressions are provided,
   --  then instead of evaluating the group or expression, the callback will
   --  be called. This is useful in particular to identify which symbols this
   --  strings relies on.

   function Write_String (Text : Text_Type) return Buffer_Slice;
   --  Writes the given string to the main text buffer, and return the slice
   --  of text that was added. Sequence of calls to Write_String will add
   --  to the buffer, essentially concatenating strings one after the other.
   --  This function is expected to be used in conjunction with
   --  Push_Buffer_Cursor and Pop_Buffer_Cursor in order to use the buffer
   --  as a temporary working memory to create text and keep track of values
   --  such as line / column.

   function Get_Empty_Slice return Buffer_Slice;
   --  Return an empty slice, still with a first value set on the current
   --  position in the text buffer.

   procedure Push_Buffer_Cursor;
   --  Pushes a new cursor to the stack of cursors. This allows subsequent
   --  calls to Write_String to update that cursor. It is expected to reset
   --  the value of Buffer.Curor.Max_Column afterwards if needed. The value
   --  will then carry the max column as strings are being written.

   procedure Pop_Buffer_Cursor;
   --  Pops the last cursor, allowing to invalidate all the text written since
   --  the last Push_Buffer_Cursor. If the Max_Column of the current cursor
   --  before the call is larger than after, then it will be copied over,
   --  as to always track the larger column encountered.

   procedure Reset_Max_Column;
   --  Resets the max column count for the current cursor. This means that
   --  the value provided by the max column field will be any max encountered
   --  between this call and the corresponding Pop_Buffer_Cursor.

   function Copy_String (Slice : Buffer_Slice) return Text_Type;
   --  Copy the slice of buffer into the returned text. Note that this is
   --  relatively slow operation - buffer slices are meant to index directly
   --  the Buffer.Str object as to avoid copies as much as possible. But when
   --  performances is not an issue, this is a nice shortcut.

   function Indent return Buffer_Slice;
   --  Write a slice representing the current indentation on the text buffer
   --  and return the indices for this slice.

   function Resolve_Indentation return Buffer_Slice;
   --  If the current buffer is on a new line, indents and returns the slice
   --  for the created indentation, otherwise returns an empty slice.

end Wrapping.Runtime.Strings;
