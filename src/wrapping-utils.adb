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
      Matcher : constant Pattern_Matcher := Compile (To_String (Pattern));
      Result  : Unbounded_Text_Type;
      Prev    : Integer         := Source'First;
      Matches : Match_Array (0 .. Paren_Count (Matcher));

      Source_Str : constant String (Source'Range) := To_String (Source);
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
