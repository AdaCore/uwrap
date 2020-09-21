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
     Post => W_Stack_Size = W_Stack_Size'Old + 1;
   --  This procedure is the entry point of the evaluation of an expression. It
   --  will keep the current value on the stack, and add the result as the
   --  top object. The status of the current context influences significantely
   --  the behavior of this procedure. In particular:
   --
   --     * Top_Context.Outer_Expr_Action: Expressions can be performed in a
   --       context where a specific action needs to be performed against the
   --       enclosing context. This can be either a match with the outer
   --       object, for example in:
   --          <some object> (<some expression>)
   --       <some expression> needs to be matched against <some object>. In
   --       this case, the outer action will do a check based on the
   --       state of Top_Context.Match_Mode and replace the top value by a
   --       Match_False if the result is false. The outer action may also
   --       be a callback to the "back" side of a command, for example:
   --          pick <some expression> wrap <some wrapping>
   --       In this case, wrapping clause is called by the outer action as
   --       well.
   --       Note that the outer action may be preserved through
   --       sub-expression calls. For example in:
   --          A.B
   --       The outer action will only be called on the resulting B. But in:
   --          A and B
   --       The outer action will be called on A and on B, which means that
   --       both A and B will go through a matching - or that both A and B will
   --       be picked.
   --
   --     * Top_Context.Yield_Callback: If this expression is called in the
   --       context of a generator, for example <some_expression>.fold (),
   --       where fold will generate all the values of <some expression> and
   --       capture them one by one, then the Evaluate_Expression will end up
   --       calling this callback, either once or for all values unless
   --       interrupted by setting Top_Context.Visit_Decision to Stop. Yield
   --       callbacks typically replace the top object by another one - the
   --       result of the expression will be the one of the last non-false
   --       yield. Yield_Callback may also call the Outer_Callback. For
   --       example, in:
   --          pick <some expression>.all () wrap <some wrapping>;
   --       The .all() will set a Yeild_Callback, getting all values generated
   --       by <some expression> and calling the Outer_Callback on each of
   --       them.
   --
   --     * Top_Context.Is_Root_Selection: In selectors such as A.B.C, it's
   --       important to track wether we're on a root selection or if we've
   --       already selected the first entity, to know if a global symbol or
   --       a member of the implicit "it" should be looked for or not.

   function Evaluate_Expression (Expr : T_Expr) return W_Object;
   --  Same as the procedure versions, but returns the result instead of
   --  pushing it to the stack.

   function Handle_Template_Call
     (Instance : W_Object; Args : T_Arg_Vectors.Vector)
      return Visit_Action with
     Post => W_Stack_Size = W_Stack_Size'Old;
   --  Given a template instance "Instance", computes the call with the
   --  arguments processed in parameters. This can be used in a walk / weave /
   --  wrap clause, or right after a template creation through a new call.

   function Push_Global_Identifier (Name : Text_Type) return Boolean with
     Post => W_Stack_Size'Old =
       (if Push_Global_Identifier'Result then W_Stack_Size - 1
          else W_Stack_Size);
   --  Looks for an identifier of the given name either on the current frame
   --  symbols or in the visible lexical scopes. Push a W_Object refering
   --  to this object if found, nothing if not found.

   procedure Execute_Expr_Outer_Action;
   --  Execute the outer action of an expression. The specific outer action
   --  to execute is controlled by Top_Context.Outer_Expr_Action.

end Wrapping.Runtime.Expressions;
