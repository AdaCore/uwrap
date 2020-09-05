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

with Ada.Wide_Wide_Text_IO;            use Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Characters.Unicode; use Ada.Wide_Wide_Characters.Unicode;
with Ada.Characters.Conversions;       use Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;  use Ada.Strings.Wide_Wide_Unbounded;

with GNAT.Regpat; use GNAT.Regpat;

package body Wrapping.Utils is

   -------------------
   -- Remove_Quotes --
   -------------------

   function Remove_Quotes (Text : Text_Type) return Text_Type is
   begin
      if Text'Length = 0 then
         return "";
      elsif Text (Text'First) /= '"' and then Text (Text'First) /= ''' then
         return Text;
      elsif Text'Length >= 6
        and then Text (Text'First .. Text'First + 2) = """"""""
      then
         return Text (Text'First + 3 .. Text'Last - 3);
      else
         return Text (Text'First + 1 .. Text'Last - 1);
      end if;
   end Remove_Quotes;

   --------------
   -- Reindent --
   --------------

   function Reindent
     (New_Indent : Integer; Text : Text_Type; Indent_First_Line : Boolean)
      return Text_Type
   is
      Space_Count        : Integer := 0;
      Spaces_To_Remove   : Integer := Integer'Last;
      Skip_To_Terminator : Boolean := False;

      Indent     : Wide_Wide_String (1 .. New_Indent) := (others => ' ');
      Line_Count : Integer                            := 1;
   begin
      for C of Text loop
         if C = ' ' then
            if not Skip_To_Terminator then
               Space_Count := Space_Count + 1;
            end if;
         elsif Is_Line_Terminator (C) then
            Space_Count        := 0;
            Skip_To_Terminator := False;
            Line_Count         := Line_Count + 1;
         else
            if Space_Count < Spaces_To_Remove then
               Spaces_To_Remove := Space_Count;
            end if;

            Skip_To_Terminator := True;
         end if;
      end loop;

      declare
         Result : Text_Type (1 .. Text'Length + Line_Count * New_Indent);
         Result_Index      : Integer := Result'First - 1;
         Characters_Before : Boolean := False;
         C                 : Wide_Wide_Character;
      begin
         if Indent_First_Line then
            Result_Index := Result_Index + 1;
            Result (Result_Index .. Result_Index + Indent'Length - 1) :=
              Indent;
            Result_Index := Result_Index + Indent'Length - 1;
         end if;

         Space_Count := 0;

         for I in Text'Range loop
            C := Text (I);

            if C = ' ' then
               Space_Count := Space_Count + 1;

               if Space_Count > Spaces_To_Remove then
                  Result_Index          := Result_Index + 1;
                  Result (Result_Index) := C;
               end if;
            elsif Is_Line_Terminator (C) then
               Space_Count           := 0;
               Result_Index          := Result_Index + 1;
               Result (Result_Index) := C;

               if I < Text'Last then
                  Result_Index := Result_Index + 1;
                  Result (Result_Index .. Result_Index + Indent'Length - 1) :=
                    Indent;
                  Result_Index := Result_Index + Indent'Length - 1;
               end if;
            elsif C /= ' ' then
               Result_Index          := Result_Index + 1;
               Result (Result_Index) := C;
            end if;
         end loop;

         --  Put_Line ("----------------------------------------"); Put_Line
         --  ("INDENT " & New_Indent'Wide_Wide_Image & ", SPACES TO REMOVE "
         --  & Spaces_To_Remove'Wide_Wide_Image); Put_Line (Text); Put_Line
         --  ("TO"); Put_Line (Result (Result'First .. Result_Index)); Put_Line
         --  ("----------------------------------------");

         return Result (Result'First .. Result_Index);
      end;
   end Reindent;

   ------------
   -- Suffix --
   ------------

   function Suffix (Text : Text_Type) return Text_Type is
   begin
      for C in reverse Text'Range loop
         if Text (C) = '.' then
            if C = Text'Last then
               return "";
            else
               return Text (C + 1 .. Text'Last);
            end if;
         end if;
      end loop;

      return Text;
   end Suffix;

   --------------------
   -- Replace_String --
   --------------------

   function Replace_String
     (Source, Pattern, Replace : Text_Type) return Text_Type
   is
      Matcher : Pattern_Matcher := Compile (To_String (Pattern));
      Result  : Unbounded_Text_Type;
      Prev    : Integer         := Source'First;
      Matches : Match_Array (0 .. Paren_Count (Matcher));

      Source_Str : String := To_String (Source);
   begin
      while Prev in Source'Range loop
         Match (Matcher, Source_Str (Prev .. Source'Last), Matches);

         if Matches (0) = No_Match then
            Append (Result, Source (Prev .. Source'Last));
            exit;
         else
            Append (Result, Source (Prev .. Matches (0).First - 1));
            Append (Result, Replace);

            Prev := Matches (0).Last + 1;
         end if;
      end loop;

      return To_Text (Result);
   end Replace_String;

end Wrapping.Utils;
