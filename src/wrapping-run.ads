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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse;

with Libadalang.Helpers; use Libadalang.Helpers;

package Wrapping.Run is

   procedure App_Post_Process
     (Context : Libadalang.Helpers.App_Context;
      Jobs    : Libadalang.Helpers.App_Job_Context_Array);

   package App is new Libadalang.Helpers.App
     ("uwrap", "Universal Code Wrapper", App_Post_Process => App_Post_Process);

   package Args is
      use GNATCOLL.Opt_Parse;

      package Input_Language is new Parse_Option
        (App.Args.Parser, "-l", "--input-language",
         "Input Language, either ada or proxy", Unbounded_String,
         Default_Val => Null_Unbounded_String);

      package Template_File is new Parse_Option
        (App.Args.Parser, "-w", "--wrapper-template",
         "Use this template wrapper file", Unbounded_String,
         Default_Val => Null_Unbounded_String);

      package Input_Directories is new Parse_Option_List
        (App.Args.Parser, "-I", "--input-dir", "Input file directory",
         Arg_Type => Unbounded_String, Accumulate => True);

   end Args;

end Wrapping.Run;
