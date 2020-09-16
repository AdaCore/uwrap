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

   Templates_To_Traverse : W_Template_Instance_Vectors.Vector;

   procedure Analyse_Input (Root_Entity : W_Node);

   procedure Analyze_Templates;

   procedure Outer_Expression_Match;

   procedure Outer_Expression_Pick;

   procedure Handle_Command_Sequence
     (Sequence : T_Command_Sequence_Element) with
     Post => W_Stack_Size = W_Stack_Size'Old;
   --  Handles a sequence of commands, assuming that the outer frame has been
   --  set, in particular with the It object and other necessary data.

   procedure Handle_Command (Command : T_Command; It : W_Node);
   --  Entry point to handle a command. Will push a frame and trigger the
   --  evaluation of various sections.

   procedure Handle_Command_In_Current_Frame (Command : T_Command);
   --  Similar to Handle_Command, but does it wihtin the current frame. It is
   --  supposed to be already set at this point.

   procedure Apply_Wrapping_Program
     (It : W_Node; Lexical_Scope : access T_Entity_Type'Class) with
     Post => W_Stack_Size = W_Stack_Size'Old;
   --  Go through the commands in the lexical scope and apply then using the
   --  It value as the currently iterated object.

end Wrapping.Runtime.Commands;
