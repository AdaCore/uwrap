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

with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package Wrapping is

   Wrapping_Error : exception;

   procedure Error (Message : Text_Type);

   procedure Warning (Message : Text_Type);

   procedure Push_Error_Location (Filename : String; Loc : Source_Location);

   procedure Pop_Error_Location;

   function Get_Sloc_Str return String;

   type Error_Callback_Type is access procedure
     (Message : Text_Type; Filename : String; Loc : Source_Location);

   Error_Callback : Error_Callback_Type;

end Wrapping;
