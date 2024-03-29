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

with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;

package body Wrapping.Runtime.Strings is

   ---------------------
   -- Evaluate_String --
   ---------------------

   function Evaluate_String (Expr : T_Expr) return Buffer_Slice is

      procedure On_Error
        (Message : Text_Type; Filename : String; Loc : Source_Location);
      --  Callback used to override the default error location when entering
      --  a string.
      --  TOTO: This is still a relatively weak way to retreive slocs for
      --  errors in strings, essentially sets the sloc at the begining of
      --  the strings, and disregards the fact that string expressions may
      --  call other functions which would have proper sloc. Lost of room
      --  for improvement.

      --------------
      -- On_Error --
      --------------

      procedure On_Error
        (Message : Text_Type; Filename : String; Loc : Source_Location)
      is
         pragma Unreferenced (Filename, Loc);
      begin
         Push_Error_Location
           (Expr.Node.Unit.Get_Filename, Start_Sloc (Expr.Node.Sloc_Range));

         Put_Line (To_Text (Get_Sloc_Str) & ": " & Message);

         raise Wrapping_Error;
      end On_Error;

      Prev_Error : Error_Callback_Type;
      Slice : Buffer_Slice := Get_Empty_Slice;

   begin
      Push_Frame_Context_No_Match;
      Top_Context.Is_Root_Selection := True;

      Prev_Error := Error_Callback;

      --  The error callback needs to be set here because the string may
      --  contain expressions which will be parsed and analyzed.
      Error_Callback := On_Error'Unrestricted_Access;

      if Expr.Str_Kind = String_Indent then
         --  In the case of an indented string, indent if we're on the begining
         --  of a line.
         Slice.Last := Resolve_Indentation.Last;
      end if;

      for Str of Expr.Str loop
         Push_Frame_Context;

         --  Each string may have its own extra indentation - add it to the
         --  current identiation.
         Top_Context.Indent := Top_Context.Indent + Str.Indent;

         case Str.Kind is
            when Str_Kind =>
               --  We're on a simple string. Add its contents to the
               --  main buffer.

               Slice.Last := Resolve_Indentation.Last;
               Slice.Last := Write_String (To_Text (Str.Value)).Last;

            when Expr_Kind =>
               --  We're on an expression. Evaluates its value and write
               --  the result in the main buffer.

               Slice.Last := Resolve_Indentation.Last;
               Slice.Last := Evaluate_Expression (Str.Expr).Write_String.Last;
               --  TODO: we systematically pop an object here. There's
               --  an optimization where we could detect that the expression
               --  is already a string, and just use the value pushed on
               --  the buffer instead.

            when Group_Kind =>
               --  We're on a group refernence. Look for that group in the
               --  current frame and push its value.

               declare
                  Position : Integer := Str.Group_Number;
                  Value    : W_Object;
               begin
                  --  Go through the various group sections. E.g. in:
                  --     match x"(.*)" do
                  --        match x"(a.*c)" do
                  --  There are two section stacked one after the other. \1
                  --  refers to (.*) and \2 refers to x"(a.*c)"

                  for C of Top_Frame.Group_Sections loop
                     if Integer (C.Groups.Length) < Position then
                        Position := Position - Integer (C.Groups.Length);
                     else
                        Value := C.Groups.Element (Position);
                        exit;
                     end if;
                  end loop;

                  if Value = null then
                     Error
                       ("cannot find group " &
                        Integer'Wide_Wide_Image (Str.Group_Number));
                  end if;

                  Slice.Last := Resolve_Indentation.Last;
                  Slice.Last := Value.Write_String.Last;
               end;
         end case;

         Pop_Frame_Context;
      end loop;

      Error_Callback := Prev_Error;
      Pop_Frame_Context;

      return Slice;
   end Evaluate_String;

   ------------------
   -- Write_String --
   ------------------

   function Write_String (Text : Text_Type) return Buffer_Slice
   is
      Result : Buffer_Slice;
   begin
      --  Set the results starting on the current cursor, and ending after
      --  the text being inserted.

      Result := (Buffer.Cursor, Buffer.Cursor);
      Result.Last.Offset := Buffer.Cursor.Offset + Text'Length - 1;

      --  Adds the text to the global buffer

      Buffer.Str (Buffer.Cursor.Offset .. Result.Last.Offset) := Text;

      --  Update the global buffer cursor

      Buffer.Cursor := Result.Last;
      Buffer.Cursor.Offset := Buffer.Cursor.Offset + 1;

      --  If we need to update cursor line / column, scan through the text
      --  content to increment and update necessary variables.

      if Buffer.Full_Cursor_Update then
         for C of Text loop
            --  TODO: This does not handle CR/LF
            if Is_Line_Terminator (C) then
               Buffer.Cursor.Line := Buffer.Cursor.Line + 1;
               Buffer.Cursor.Line_Offset := 1;
               Buffer.Cursor.Column := 1;
            else
               Buffer.Cursor.Line_Offset := Buffer.Cursor.Line_Offset + 1;
               --  TODO : This does not handle tabs
               Buffer.Cursor.Column := Buffer.Cursor.Column + 1;

               if Buffer.Cursor.Max_Column < Buffer.Cursor.Column then
                  Buffer.Cursor.Max_Column := Buffer.Cursor.Column;
               end if;
            end if;
         end loop;
      end if;

      return Result;
   end Write_String;

   ---------------------
   -- Get_Empty_Slice --
   ---------------------

   function Get_Empty_Slice return Buffer_Slice is
      Result : Buffer_Slice := (Buffer.Cursor, Buffer.Cursor);
   begin
      --  An empty slice is set to start on the current buffer custor but with
      --  an tempty length.

      Result.Last.Offset := Result.First.Offset - 1;
      Result.Last.Line := Result.First.Line - 1;
      Result.Last.Line_Offset := Result.First.Line_Offset - 1;
      Result.Last.Column := Result.First.Column - 1;

      return Result;
   end Get_Empty_Slice;

   ------------------------
   -- Push_Buffer_Cursor --
   ------------------------

   procedure Push_Buffer_Cursor is
   begin
      Buffer.Cursor_Stack.Append (Buffer.Cursor);
   end Push_Buffer_Cursor;

   -----------------------
   -- Pop_Buffer_Cursor --
   -----------------------

   procedure Pop_Buffer_Cursor is
      Max_Column : constant Integer := Buffer.Cursor.Max_Column;
   begin
      Buffer.Cursor := Buffer.Cursor_Stack.Last_Element;
      Buffer.Cursor_Stack.Delete_Last;

      if Max_Column > Buffer.Cursor.Max_Column then
         --  When popping a buffer cursor, we still want to track the maximum
         --  column encountered so far. Update the parent cursor if the
         --  new section of text found a larger value.

         Buffer.Cursor.Max_Column := Max_Column;
      end if;
   end Pop_Buffer_Cursor;

   ----------------------
   -- Reset_Max_Column --
   ----------------------

   procedure Reset_Max_Column is
   begin
      Buffer.Cursor.Max_Column := 0;
   end Reset_Max_Column;

   -----------------
   -- Copy_String --
   -----------------

   function Copy_String (Slice : Buffer_Slice) return Text_Type is
   begin
      return Buffer.Str (Slice.First.Offset .. Slice.Last.Offset);
   end Copy_String;

   ------------
   -- Indent --
   ------------

   function Indent return Buffer_Slice is
   begin
      return Write_String
        (Text_Type'(1 .. Top_Context.Indent => ' '));
   end Indent;

   -------------------------
   -- Resolve_Indentation --
   -------------------------

   function Resolve_Indentation return Buffer_Slice is
   begin
      if Buffer.Cursor.Offset > 1
        and then Is_Line_Terminator (Buffer.Str (Buffer.Cursor.Offset - 1))
      then
         --  If we're on a begining of a line, provide identiation.

         return Indent;
      else
         --  We're not on a beginign of a line, no identation.

         return Get_Empty_Slice;
      end if;
   end Resolve_Indentation;

end Wrapping.Runtime.Strings;
