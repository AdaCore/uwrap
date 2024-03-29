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

--  This package implements some of the intrinsic functions provided by the
--  language. These functions are aimed at being stacked with the object
--  W_Function_Instrinsic.

with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;

package Wrapping.Runtime.Functions is

   procedure Call_Normalize_Ada_Name
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);
   --  Takes the first parameter as an id in an arbitrary naming convention,
   --  and make it follow the Ada naming style as much as possible.

   procedure Call_Replace_Text
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);
   --  Takes three parameters, source, pattern and replace by. Replaces all
   --  occurences of the regular expression pattern in source by replace by.
   --  TODO: It would be useful to differenciate strings and regexpr in
   --  the pattern param

   procedure Call_To_Lower
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);
   --  Takes one string parameter and returns its lowercase version

   procedure Call_Max_Col
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);
   --  Takes one parameters which is an expression. Returns the maximum column
   --  number that occured during this expression evaluation.

   procedure Call_Convert_To_Text
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);
   --  Convert the parameter to its text version.

end Wrapping.Runtime.Functions;
