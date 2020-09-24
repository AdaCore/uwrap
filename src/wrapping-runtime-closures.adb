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

with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;
with Wrapping.Runtime.Objects;     use Wrapping.Runtime.Objects;

package body Wrapping.Runtime.Closures is

   ---------------------
   -- Capture_Closure --
   ---------------------

   function Capture_Closure (Names : Text_Sets.Set) return Closure is
      A_Closure : constant Closure := new Closure_Type;
   begin
      Push_Frame_Context;
      Top_Context.Is_Root_Selection := True;

      for Name of Names loop
         if Push_Global_Identifier (Name) then
            if Top_Object.Dereference.all in W_Static_Entity_Type'Class
              or else Top_Object.Dereference.all in W_Function_Type'Class
            then
               --  We don't capture static references, they can later be
               --  retreived from context. Genreating symbols for them would
               --  also confused name resolution as we would have a symbol and
               --  a statically solvable name.
               Pop_Object;
            elsif Top_Object.all in W_Reference_Type'Class
              and then W_Reference (Top_Object).Is_Implicit_It
            then
               --  We don't want to carry the It property over to the deferred
               --  call, so remove it.

               A_Closure.Captured_Symbols.Insert
                 (Name,
                  new W_Reference_Type'
                    (Value => W_Reference (Pop_Object).Value, others => <>));
            else
               A_Closure.Captured_Symbols.Insert (Name, Pop_Object);
            end if;
         end if;
      end loop;

      A_Closure.Implicit_It   := Get_Implicit_It;
      A_Closure.Lexical_Scope := Top_Frame.Lexical_Scope;
      A_Closure.Temp_Names    := Top_Frame.Temp_Names;
      A_Closure.Left_Value    := Top_Context.Left_Value;

      return A_Closure;
   end Capture_Closure;

end Wrapping.Runtime.Closures;
