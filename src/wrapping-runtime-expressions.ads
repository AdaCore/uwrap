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

with Ada.Containers;              use Ada.Containers;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Frames;     use Wrapping.Runtime.Frames;

package Wrapping.Runtime.Expressions is

   procedure Evaluate_Expression (Expr : T_Expr) with
     Post => Top_Frame.Data_Stack.Length =
       Top_Frame.Data_Stack.Length'Old + 1;

   function Evaluate_Expression (Expr : T_Expr) return W_Object;

   function Handle_Template_Call
     (Instance : W_Object; Args : T_Arg_Vectors.Vector)
      return Visit_Action with
     Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;

   function Push_Global_Identifier (Name : Text_Type) return Boolean with
     Post => Top_Frame.Data_Stack.Length'Old =
       (if Push_Global_Identifier'Result then Top_Frame.Data_Stack.Length - 1
        else Top_Frame.Data_Stack.Length);

end Wrapping.Runtime.Expressions;
