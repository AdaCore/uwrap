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

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Frames;     use Wrapping.Runtime.Frames;

package Wrapping.Runtime.Matching is

   procedure Push_Match_True (An_Entity : access W_Object_Type'Class);

   procedure Push_Match_False;

   function Evaluate_Match
     (Matching_Expression : T_Expr;
      Object : W_Object := Top_Object) return Boolean;

   procedure Push_Match_Result
     (Matching_Expression : T_Expr;
      Object : W_Object := Top_Object);

   function Match (Pattern, Text : Text_Type) return Boolean;
   --  Match a pattern with a text, adding group and captured variables on the
   --  top frame

   procedure Evaluate_Generator_Regexp
     (Root      : access W_Object_Type'Class;
      Expr      : T_Expr;
      Generator : Generator_Type);

   Match_False : constant W_Object := new W_Object_Type;
   --  TODO: There is a potential confusion at some point. The usage of
   --  Match_False prevents in many cases the usage of a regular false boolean
   --  as a valid result - essentially meaning that booleans as objects are not
   --  supported. For example, when generating values from a container, the
   --  result of the generation will be the last non-Match_False value. We may
   --  want to introduce a proper W_Boolean type at some point, and make sure
   --  to differenciate it from Match_False when needed.

end Wrapping.Runtime.Matching;
