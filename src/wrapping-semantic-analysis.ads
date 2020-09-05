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

with Ada.Containers.Indefinite_Vectors;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;

package Wrapping.Semantic.Analysis is

   Root : Semantic.Structure.T_Namespace :=
     new Semantic.Structure.T_Namespace_Type;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   Files : String_Vectors.Vector;

   procedure Load_Module (Path : String; Name : String);

   procedure Analyze;

   procedure Push_Error_Location (Node : Template_Node'Class);

end Wrapping.Semantic.Analysis;
