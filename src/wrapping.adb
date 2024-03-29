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

with Ada.Containers.Vectors;     use Ada.Containers;
with Ada.Directories;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Wide_Wide_Text_IO;      use Ada.Wide_Wide_Text_IO;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

package body Wrapping is

   type Error_Location is record
      Filename : Unbounded_String;
      Loc      : Source_Location;
   end record;

   package Error_Location_Vector is new Ada.Containers.Vectors
     (Positive, Error_Location);

   Error_Stack : Error_Location_Vector.Vector;

   ------------------
   -- Get_Sloc_Str --
   ------------------

   function Get_Sloc_Str return String is
   begin
      if Error_Stack.Length > 0 then
         return
           To_String (Error_Stack.Last_Element.Filename) & ":" &
           Trim (Error_Stack.Last_Element.Loc.Line'Image, Left) & ":" &
           Trim (Error_Stack.Last_Element.Loc.Column'Image, Left);
      else
         return "";
      end if;
   end Get_Sloc_Str;

   -----------
   -- Error --
   -----------

   procedure Error (Message : Text_Type) is
   begin
      if Error_Callback /= null then
         Error_Callback.all
           (Message, To_String (Error_Stack.Last_Element.Filename),
            Error_Stack.Last_Element.Loc);
      else
         if Error_Stack.Length > 0 then
            Put_Line (To_Text (Get_Sloc_Str) & ": " & Message);
         else
            Put_Line (Message);
         end if;

         raise Wrapping_Error;
      end if;
   end Error;

   -------------
   -- Warning --
   -------------

   procedure Warning (Message : Text_Type) is
   begin
      if Error_Stack.Length > 0 then
         Put_Line (To_Text (Get_Sloc_Str) & ": warning: " & Message);
      else
         Put_Line ("warning: " & Message);
      end if;
   end Warning;

   -------------------------
   -- Push_Error_Location --
   -------------------------

   procedure Push_Error_Location (Filename : String; Loc : Source_Location) is
   begin
      if Filename = "<no source>" then
         Error_Stack.Append
           (Error_Location'(To_Unbounded_String ("<no source>"), Loc));
      else
         Error_Stack.Append
           (Error_Location'
              (To_Unbounded_String (Ada.Directories.Simple_Name (Filename)),
               Loc));
      end if;
   end Push_Error_Location;

   ------------------------
   -- Pop_Error_Location --
   ------------------------

   procedure Pop_Error_Location is
   begin
      Error_Stack.Delete_Last;
   end Pop_Error_Location;

end Wrapping;
