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

package Wrapping.Runtime is

   --  The children of this package execute a UWrap program that has been
   --  parsed and analyzed by the Wrapping.Semantic children package. The
   --  algorithm works at the high level as follows:
   --
   --  UWrap iterates over a series of node - at first through a deep-traversal
   --  tree on the input tree in Analysis.Analyze_Input, then through the list
   --  of instantiated templates in Analsis.Analyze_Templates.
   --
   --  On each node, the callback Analysis.Analysis_Visitor will be called. Its
   --  role is to setup the global frame for the analysis and to run the
   --  wrapping program on each node.
   --
   --  The procedure Analyze.Apply_Wrapping_Program is applying the wrapping
   --  program on the current node. It will go through all commands in the
   --  root lexical scope and apply them in reverse order, so that last
   --  commands can override earlier ones.
   --
   --  A command is applied on the current node by the Analyze.Handle_Command
   --  call. This is then decomposed in the following steps:
   --     - Analyze.Handle_Command_Font will handle store the command in the
   --       list of defered_commands, or call directly the next stage.
   --     - Analyze.Handle_Command_Fontnodefer is the second stage. It is
   --       is called either directly by Handle_Command_Front, or once a
   --       deferred command is ready to be executed. It will evaluate the
   --       match clause if any. From then:
   --          - If matched and there's no pick clause, it will execute the
   --            end of the command, Analysis.Handle_Command_Back
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
   --    - Analysis.Analyze_Command_Back is the third stage. It can be called
   --      directly from Analyze_Command_Nodefer, from the pick
   --      Outer_Expr_Callback or from Handle_Command_Sequence (see later). It
   --      first adjust the value on the top of the stack and raises an error
   --      if it can't be executed. It will then either stop there if there's
   --      no more clause, execute the wrap, weave or walk clause through the
   --      Analyzis.Apply_Template_Action, or run the sequence of command
   --      through Analyzis.Handle_Command_Sequence
   --    - Analyzis.Handle_Command_Sequence is the last procedure able to
   --      schedule commands. It will iterate over all the sections separated
   --      by a 'then' in order up until hitting either the end of the sequence
   --      or an 'else' or 'elsmatch. Within this section, it will evaluate
   --      variables declaration, then execute command in reverse order,
   --      calling back to Analysis.Handle_Command_Front.
   --
   --   Another important entry point is Analysis.Evalutate_Expression, which
   --   is the call evaluating any expression or subexpression. Its role is
   --   to put a value (a W_Object) on the stack of values depending on the
   --   expression passed in paramters.
   --
   --   Evaluating commands and expressions requires passing data and states
   --   through the environment. The main entry point to these is the type
   --   Structure.Data_Frame, which contains states and information global to
   --   a frame, either the global one or the one of a function or template
   --   instance. This contains in particular the data stack as well as various
   --   other states
   --
   --   A frame also contains a pointer to the current context, held by the
   --   type Structure.Data_Frame_Context. This type evolves much faster
   --   through the analysis, and is typically used to pass local information
   --   across calls. For example, during the analysis of a selected
   --   expression, it will track wether the sub-expression is the first token
   --   or one after a dot.

end Wrapping.Runtime;
