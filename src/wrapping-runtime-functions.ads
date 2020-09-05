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

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Objects;    use Wrapping.Runtime.Objects;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;

package Wrapping.Runtime.Functions is

   procedure Call_Normalize_Ada_Name
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   procedure Call_Replace_Text
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   procedure Call_To_Lower
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   procedure Call_Reindent
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

end Wrapping.Runtime.Functions;
