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

--  This package wraps around the standard regular expressions and provides
--  capabilities to name matching groups.
--  TODO: while the result of UWrap is Wide_Wide_String, this package only
--  supports 8 bits strings, as there's no other regexpr in standard Ada
--  library. This will create bugs when matching real world files using other
--  charset and should be fixed at some point - possibly by duplicating the
--  GNAT unit and using Text_Type instead of strings as a first implementation.

with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Regpat; use GNAT.Regpat;

package Wrapping.Regex is

   Unknown_Group_Name : exception;
   --  Exception raised when trying to get a group by name that doesn't exist

   Bad_Group_Index    : exception;
   --  Exception raised when trying to get a group by index that doesn't exist

   type Basic_Regex is private;
   --  This type encompases a regular expression and the names of its capture
   --  groups

   package Match_Holder is new Ada.Containers.Indefinite_Holders (Match_Array);

   type Capture_Group is record
      Name     : Unbounded_String;
      Index    : Natural;
      Location : Match_Location;
   end record;
   --  Stores a group that has been captured, with its name if provided

   package Group_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Capture_Group);

   type Match_Obj is record
      Matches         : Match_Holder.Holder;
      Groups          : Group_Vectors.Vector;
      Original_String : Unbounded_String;
   end record;
   --  Stores the result of a matching operations

   package Match_List_Container is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Match_Obj);

   function Compile (Pattern : String) return Basic_Regex;
   --  Create a regexpr after a specific pattern. On top of the usual regexpr
   --  semantic, users can name a group with angular brackets, e.g.:
   --    "(<name>.*)"
   --  Groups can then be retreived by position or by name.

   function Match (Self : Basic_Regex; Str : String) return Match_Obj;
   --  Run the regexp against the string and returns all the match groups,

   function No_Match (Self : Match_Obj) return Boolean;
   --  Return trues if the match obj in parameter represent a no-matching
   --  situation, false otherwise

   function Length (Self : Match_Obj) return Natural;
   --  Return the number of groups in ths match object

   function Get (Self : Match_Obj; Name : String) return String;
   --  Return the content of the text captured on the name in parameter

   function Get (Self : Match_Obj; Index : Natural) return String;
   --  Return the content of the text captured at the index in parameter

   function Get_Capture_Name (Self : Match_Obj; Index : Natural) return String;
   --  Return the name for the group at the given index, empty string if none.

private

   package Pattern_Holder is new Ada.Containers.Indefinite_Holders
     (Pattern_Matcher);

   type Basic_Regex is record
      Groups  : Group_Vectors.Vector;
      Pattern : Pattern_Holder.Holder;
   end record;

   No_Group_Name : constant String := "";

   function Get_Noexcept (Self : Match_Obj; Index : String) return String;
end Wrapping.Regex;
