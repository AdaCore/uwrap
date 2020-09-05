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
-- General Public License  distributed with SVD2Ada; see file COPYING3.  If --
-- not, go to http://www.gnu.org/licenses for a complete copy of the        --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with GNAT.Regpat; use GNAT.Regpat;

package Wrapping.Regex is

   Unknown_Group_Name : exception;
   Bad_Group_Index    : exception;

   type Basic_Regex is private;

   package Match_Holder is new Ada.Containers.Indefinite_Holders (Match_Array);

   type Capture_Group is record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Index    : Natural;
      Location : GNAT.Regpat.Match_Location;
   end record;

   package Name_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Capture_Group);

   type Match_Obj is record
      Matches         : Match_Holder.Holder;
      Names           : Name_Vectors.Vector;
      Original_String : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Match_List_Container is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Match_Obj);

   function Compile (Pattern : String) return Basic_Regex;

   function Match (Self : Basic_Regex; Str : String) return Match_Obj;

   function No_Match (Self : Match_Obj) return Boolean;

   function Length (Self : Match_Obj) return Natural;

   function Get (Self : Match_Obj; Index : String) return String;

   function Get (Self : Match_Obj; Index : Natural) return String;

   function Get_Capture_Name (Self : Match_Obj; Index : Natural) return String;

private

   package Pattern_Holder is new Ada.Containers.Indefinite_Holders
     (Pattern_Matcher);

   type Basic_Regex is record
      Names   : Name_Vectors.Vector;
      Pattern : Pattern_Holder.Holder;
   end record;

   No_Group_Name : constant String := "";

   function Get_Noexcept (Self : Match_Obj; Index : String) return String;
end Wrapping.Regex;
