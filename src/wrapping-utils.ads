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

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

package Wrapping.Utils is

   package Text_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, Text_Type);

   type Text_Maps_Access is access all Text_Maps.Map;

   package Text_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Text_Type);

   function Remove_Quotes (Text : Text_Type) return Text_Type;
   --  Remove the quotes before and after the Text, taking into account both
   --  the single line syntax and the multi lines one.

   package Text_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Text_Type);

   function Suffix (Text : Text_Type) return Text_Type;
   --  Returns the suffix of a dotted name, or the input if no dot.

   function Replace_String
     (Source, Pattern, Replace : Text_Type) return Text_Type;
   --  Replace the in the Source parameter the Pattern interpreted as a regular
   --  expression by the text in Replace.

end Wrapping.Utils;
