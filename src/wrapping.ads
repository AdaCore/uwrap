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

with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package Wrapping is

   Wrapping_Error : exception;
   --  Exception to be raised when an error appears during either
   --  pre-processing or the UWrap program or its execution.

   procedure Error (Message : Text_Type);
   --  Calls the error callback if set, otherwise prints the message on the
   --  console with the error sloc before and raise Wrapping Error.

   procedure Warning (Message : Text_Type);
   --  Prints the message on the console with the error sloc before and raise
   --  Wrapping Error.

   procedure Push_Error_Location (Filename : String; Loc : Source_Location);
   --  Push an error location. All errors and wranings from this on will be
   --  associated to this location.

   procedure Pop_Error_Location;
   --  Pops the previous error location, moving back to the one that was pushed
   --  before.

   function Get_Sloc_Str return String;
   --  Constructs a string corresponding to the current error location.

   type Error_Callback_Type is access procedure
     (Message : Text_Type; Filename : String; Loc : Source_Location);
   --  Override the default error procedure behavior.

   Error_Callback : Error_Callback_Type;
   --  When set, Error will call the error callback instead of doing a direct
   --  write & raise.

end Wrapping;
