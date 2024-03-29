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
with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Frames;     use Wrapping.Runtime.Frames;

package Wrapping.Runtime.Matching is

   procedure Push_Match_True (An_Entity : access W_Object_Type'Class);
   --  Push a match true. Any non Match_False value is considered to be true,
   --  so this is equivalent to pushing directly the entity.

   procedure Push_Match_False;
   --  Push a Match_False on the stack.

   procedure Push_Match_Result
     (Matching_Expression : T_Expr; Object : W_Object := Top_Object);
   --  Matches the expression against the object in parameter. Pushes that
   --  object if true, Match_False otherwise.

   function Evaluate_Match
     (Matching_Expression : T_Expr;
      Object : W_Object := Top_Object) return Boolean;
   --  Same as Push_Match_Result, but returns wether or not a match has been
   --  found as opposed to pushing the result on the stack

   function Match (Pattern, Text : Text_Type) return Boolean;
   --  Match a pattern with a text, adding group and captured variables on the
   --  top frame

   procedure Evaluate_Generator_Regexp
     (Root      : access W_Object_Type'Class;
      Expr      : T_Expr;
      Generator : Generator_Callback_Type);
   --  Evaluates a regular expression against values generated by a specific
   --  function and a root object. For example:
   --     a \ b \ many (c)
   --  The Generator must be a function that process the implicit "it" value
   --  and pushes a value that can then be re-used by the same generator, so
   --  that the previous example means:
   --     (1) runs the generator on it and matches the result against a
   --     (2) runs the generator on the previous result and matches its result
   --         against b
   --     (3) recursively runs the generator on the previous result and match
   --         the result against c as long as possible.
   --   The value pushed on the stack is either Match_False if the regular
   --   expression could not find any matches, or a W_Regexpr_Result_Type that
   --   contains the sequence of objects that were matched.

   Match_False : constant W_Object := new W_Object_Type;
   --  TODO: There is a potential confusion at some point. The usage of
   --  Match_False prevents in many cases the usage of a regular false boolean
   --  as a valid result - essentially meaning that booleans as objects are not
   --  supported. For example, when generating values from a container, the
   --  result of the generation will be the last non-Match_False value. We may
   --  want to introduce a proper W_Boolean type at some point, and make sure
   --  to differenciate it from Match_False when needed.

end Wrapping.Runtime.Matching;
