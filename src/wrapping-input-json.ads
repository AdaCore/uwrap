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

--  This package implements a tree structure for JSON nodes based on the
--  GNATColl implementation.

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Wrapping.Runtime.Nodes;     use Wrapping.Runtime.Nodes;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;

package Wrapping.Input.JSON is

   type W_JSON_Node_Type;
   type W_JSON_Node is access all W_JSON_Node_Type'Class;

   type W_JSON_Node_Type is new W_Node_Type with record
      Node : JSON_Value;
      Name : Unbounded_Text_Type;
   end record;
   --  Base node type for JSON nodes.

   overriding function Push_Value
     (An_Entity : access W_JSON_Node_Type; Name : Text_Type) return Boolean;
   --  Pushes the value for one of the three instrinsic functions defined for
   --  JSON (kind, name and value) or calls parent.

   overriding function Write_String
     (Object : W_JSON_Node_Type) return Buffer_Slice;
   --  Writes an empty string (there's no string correspondance to JSON nodes)
   --  TODO: witing the contents of the node would be a useful alternative and
   --  allow text regexpr on the entire text.

   overriding function To_Debug_String
     (Object : W_JSON_Node_Type) return Text_Type;
   --  See parent documentation

   overriding function Language (Object : W_JSON_Node_Type) return Text_Type is
     ("json");
   --  See parent documentation

   procedure Analyze_File (Filename : String);
   --  Takes the file in parameter, parse the JSON contents and run the uwrap
   --  program on it. Deferred commands still need to be analyzed, presumably
   --  once all files are analyzed.

end Wrapping.Input.JSON;
