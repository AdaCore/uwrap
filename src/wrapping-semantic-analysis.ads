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

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;

package Wrapping.Semantic.Analysis is

   Root : constant T_Namespace := new T_Namespace_Type;
   --  This is the root of the entire UWrap program. All entities, starting
   --  with all loaded files, are children of this object.

   procedure Load_Module (Path : String; Name : String);
   --  Loads the module located under the given path. The name is the dotted
   --  name of this module.

   procedure Analyze;
   --  Once all modules have been loaded, analyze their contents and build the
   --  remaining information to make the program ready to run.

   procedure Push_Error_Location (Node : Template_Node'Class);
   --  Push an error location after a Node. See functions declared in the
   --  package Wrapping for more details on error management.

end Wrapping.Semantic.Analysis;
