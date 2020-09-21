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

with Ada.Containers; use Ada.Containers;

with Libtemplatelang;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Objects;    use Wrapping.Runtime.Objects;
with Wrapping.Utils;              use Wrapping.Utils;

package Wrapping.Runtime.Commands is

   procedure Analyse_Input (Root_Entity : W_Node);
   --  This function will go through a a deep-traversal tree on the node,
   --  On each node, Apply_Wrapping_Program will be called, executing tyhe
   --  wrapping program

   procedure Analyzed_Deferred;
   --  Runs analysis following input processing, that is:
   --     - Runs the wrapping program on generated templates
   --     - Evaluates and run deferred commands

   procedure Handle_Command (Command : T_Command; It : W_Node);
   --  Entry point to handle a command. Will push a frame and trigger the
   --  evaluation of various sections. This function will first set a frame
   --  for the command, following by the given steps:
   --     - Handle_Command_In_Current_Frame will handle store the command in
   --       the list of defered_commands, or call directly the next stage.
   --     - Handle_Command_Post_Defer is the second stage. It is
   --       is called either directly by Handle_Command_In_Current_Frame, or
   --       once a deferred command is ready to be executed. It will evaluate
   --       the match clause if any. From then:
   --          - If matched and there's no pick clause, it will execute the
   --            end of the command, Handle_Command_Back
   --          - If matched but there's a pick clause, the expression of that
   --            pick clause will be executed. The Pick clause will be
   --            responsible to call the back command once or several times
   --            depending on how many values it picks. This call will be done
   --            though the callback registered under the Outer_Expr_Callback
   --            of the stacked context.
   --          - If not matched but there's a command sequence, and if that
   --            command sequence, it will go through the elsematch and else
   --            sections to find a subsequence to execute. If found, it will
   --            be executed through the Analysis.Handle_Command_Sequence call.
   --    - Analyze_Command_Post_Pick is the third stage. It can be called
   --      directly from Handle_Command_Nodefer, from the pick
   --      Outer_Expr_Callback or from Handle_Command_Sequence (see later). It
   --      first adjust the value on the top of the stack and raises an error
   --      if it can't be executed. It will then either stop there if there's
   --      no more clause, execute the wrap, weave or walk clause through the
   --      Apply_Template_Action, or run the sequence of command through
   --      Analyzis.Handle_Command_Sequence
   --    - Handle_Command_Sequence is the last procedure able to
   --      schedule commands. It will iterate over all the sections separated
   --      by a 'then' in order up until hitting either the end of the sequence
   --      or an 'else' or 'elsmatch. Within this section, it will evaluate
   --      variables declaration, then execute command in reverse order,
   --      calling back to Analysis.Handle_Command_Front.

   procedure Handle_Command_In_Current_Frame (Command : T_Command);
   --  Similar to Handle_Command, but does it wihtin the current frame. It is
   --  supposed to be already set at this point.

   procedure Handle_Command_Post_Pick (Command : T_Command) with
     Post => W_Stack_Size = W_Stack_Size'Old;
   --  Handle a command after the execution of a pick clause. Note that a pick
   --  clause can generate more than one value, this function may be called
   --  more than once for a given pick clause.

   procedure Handle_Command_Sequence
     (Sequence : T_Command_Sequence_Element) with
     Post => W_Stack_Size = W_Stack_Size'Old;
   --  Handles a sequence of commands, assuming that the outer frame has been
   --  set, in particular with the It object and other necessary data.

   procedure Apply_Wrapping_Program
     (It : W_Node; Lexical_Scope : access T_Entity_Type'Class) with
     Post => W_Stack_Size = W_Stack_Size'Old;
   --  This procedure is applying the wrapping program on the current node. It
   --  will go through all commands in the root lexical scope and apply them in
   --  reverse order, so that last commands can override earlier ones.

   procedure Register_Template_Instance (Instance : W_Template_Instance);
   --  Registers a template instance to be processed later by
   --  Analyzed_Deferred.

end Wrapping.Runtime.Commands;
