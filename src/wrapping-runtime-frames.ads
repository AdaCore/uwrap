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

package Wrapping.Runtime.Frames is

   function Top_Object return W_Object with Inline;
   --  Return the object at the top of the current stack.

   procedure Push_Frame (Lexical_Scope : access T_Entity_Type'Class);

   procedure Push_Frame (Frame : Data_Frame);

   procedure Push_Frame (A_Closure : Closure);

   procedure Pop_Frame;

   procedure Push_Frame_Context;

   procedure Push_Frame_Context (Context : Frame_Context_Type);

   procedure Push_Frame_Context_Parameter;

   procedure Push_Frame_Context_Parameter_With_Match (Object : W_Object);

   procedure Push_Frame_Context_No_Outer;

   procedure Push_Frame_Context_No_Match;

   procedure Push_Frame_Context_No_Pick;
   --  Many expression part need to deactivate being picked as function
   --  results. For example, while in:
   --     function f do
   --        pick a and b;
   --     end;
   --  we want to pick a and b (if it's called from e.g. .all(), in :
   --     function f do
   --        pick a & b;
   --     end;
   --  we only want to pick the result of a & b. The following function pushes
   --  a new context with the proper flags.

   procedure Pop_Frame_Context;

   procedure Push_Match_Groups_Section;

   procedure Pop_Match_Groups_Section;

   procedure Push_Object (Object : access W_Object_Type'Class);

   procedure Push_Implicit_It (Object : access W_Object_Type'Class);

   procedure Push_Allocated_Entity (Object : access W_Object_Type'Class);

   procedure Push_Temporary_Name (Name : Text_Type; Counter : in out Integer);
   --  Push the temporary of the given name on the stack. If one needs to be
   --  created, counter will be used to append to the name and be incremented.

   procedure Pop_Object (Number : Positive := 1);

   procedure Delete_Object_At_Position (Position : Integer) with
     Pre => Position /= 0;
   --  Deletes a specific object. Negative deletes from end, positives delete
   --  from start

   function Pop_Object return W_Object;

   function Top_Is_Implicit return Boolean;
   --  Return True if the top object on the frame is an implicitely stacked
   --  object

   function Get_Implicit_It (From : Data_Frame := Top_Frame) return W_Object;

private

   Top_Object_Ref : W_Object;

   function Top_Object return W_Object is (Top_Object_Ref);

end Wrapping.Runtime.Frames;
