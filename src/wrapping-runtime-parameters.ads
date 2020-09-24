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

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;

package Wrapping.Runtime.Parameters is

   type Parameter is record
      Name        : Unbounded_Text_Type;
      Is_Optional : Boolean;
   end record;
   --  Models a parameter used for e.g. functions calls

   type Parameter_Profile is array (Positive range <>) of Parameter;
   --  Models a profile for a function

   type Actual_Expressions is array (Positive range <>) of T_Expr;
   --  Returns the expressions used to value a call

   function Make_Parameter
     (Name : Text_Type; Is_Optional : Boolean) return Parameter;
   --  Creates a parameter object

   function Process_Parameters
     (Profile : Parameter_Profile; Arg : T_Arg_Vectors.Vector)
      return Actual_Expressions;
   --  Process the parameters according to a profile and a set of actual
   --  arguments. The resulting actual expression as as many expressions as
   --  for the profile, with null for optional parameters not valuated.
   --  Parameters in Arg have to be provided first as positional, then as
   --  named convention - not respecting this order will lead to an error.

   procedure Process_Parameters
     (Args               : T_Arg_Vectors.Vector;
      Evaluate_Parameter : access procedure
        (Name : Text_Type; Position : Integer; Value : T_Expr));
   --  Takes the argument one by one, ensuring that they are provided first
   --  as positional then as named notation. This is useful when there's no
   --  formal profile, for example with templates

end Wrapping.Runtime.Parameters;
