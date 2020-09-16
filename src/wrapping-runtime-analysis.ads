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

package Wrapping.Runtime.Analysis is

   Templates_To_Traverse : W_Template_Instance_Vectors.Vector;

   Deferred_Commands : Deferred_Command_Vectors.Vector;

   procedure Analyse_Input (Root_Entity : W_Node);

   procedure Analyze_Templates;

   function Analyze_Visitor
     (E : access W_Object_Type'Class; Result : out W_Object)
      return Wrapping.Semantic.Structure.Visit_Action;

   Visitor_Counter : Integer := 0;
   --  This is the counter of visitor. Every time a visitor is started
   --  (including the main one), it is to be incremented. This provdes a unique
   --  id to each visit execution, which later allows to check that a language
   --  entity isn't visited twice by the same visitor invokation.

   Current_Visitor_Id : Integer := 0;
   --  The Id for the current visitor, updated when entering a vistor
   --  invokation. Note that the main iteration is always id 0. TODO:
   --  maybe this should be frame information?

   procedure Outer_Expression_Match;

   procedure Outer_Expression_Pick;

   procedure Handle_Command_Sequence
     (Sequence : T_Command_Sequence_Element) with
     Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;

   procedure Handle_Command_Front (Command : T_Command) with
     Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;

   procedure Apply_Wrapping_Program
     (It : W_Node; Lexical_Scope : access T_Entity_Type'Class) with
     Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;

end Wrapping.Runtime.Analysis;
