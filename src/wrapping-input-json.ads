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

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Wrapping.Runtime.Objects;   use Wrapping.Runtime.Objects;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;

package Wrapping.Input.JSON is

   type W_JSON_Node_Type;
   type W_JSON_Node is access all W_JSON_Node_Type'Class;

   type W_JSON_Node_Type is new W_Node_Type with record
      Node : JSON_Value;
      Name : Unbounded_Text_Type;
   end record;

   overriding function Push_Value
     (An_Entity : access W_JSON_Node_Type; Name : Text_Type) return Boolean;

   overriding function Write_String
     (Object : W_JSON_Node_Type) return Buffer_Slice;

   overriding function To_Debug_String
     (Object : W_JSON_Node_Type) return Text_Type;

   overriding function Language (Object : W_JSON_Node_Type) return Text_Type is
     ("json");

   procedure Analyze_File (Filename : String);

end Wrapping.Input.JSON;
