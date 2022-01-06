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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with GNATCOLL.OS.Constants; use GNATCOLL.OS.Constants;
with GNATCOLL.Utils;        use GNATCOLL.Utils;

with Wrapping.Semantic;          use Wrapping.Semantic;
with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Wrapping.Runtime.Commands;  use Wrapping.Runtime.Commands;
with Wrapping.Input.JSON;        use Wrapping.Input.JSON;
with Wrapping.Runtime.Strings;   use Wrapping.Runtime.Strings;

package body Wrapping.Run is

   ----------------------
   -- App_Post_Process --
   ----------------------

   procedure App_Post_Process
     (Context : Libadalang.Helpers.App_Context;
      Jobs    : Libadalang.Helpers.App_Job_Context_Array)
   is
      pragma Unreferenced (Context);
   begin
      --  Initialize main buffer
      --  TODO: Size should be parametrizable.

      --  Create a buffer of about 1 M of characters, should be enough for most
      --  usages by default.
      Buffer.Str := new Text_Type (1 .. 2 ** 20);

      --  Step 1, load the templates

      if Args.Template_File.Get = "" then
         Put_Line ("Error: missing template file");
         return;
      end if;

      declare
         procedure Analyze_Directory
           (Base_Module_Name : String; Dir_Path : String);

         Input_Directories : constant Args.Input_Directories.Result_Array :=
           Args.Input_Directories.Get;

         -----------------------
         -- Analyze_Directory --
         -----------------------

         procedure Analyze_Directory
           (Base_Module_Name : String; Dir_Path : String)

         is
            Dir      : Dir_Type;
            File     : String (1 .. 2_048);
            Name_Len : Integer;

            Container_Module : constant String :=
              (if Base_Module_Name /= "" then Base_Module_Name & "." else "");
         begin
            Open (Dir, Dir_Path);

            loop
               Read (Dir, File, Name_Len);

               exit when Name_Len = 0;

               declare
                  Full_Path : constant String :=
                    Dir_Path & File (File'First .. Name_Len);
               begin
                  if File_Extension (File (File'First .. Name_Len)) = ".wrp"
                  then
                     declare
                        Module_Name : constant String :=
                          Container_Module &
                          Base_Name (File (File'First .. Name_Len), ".wrp");
                     begin
                        Load_Module (Full_Path, Module_Name);
                     end;
                  elsif File (File'First .. Name_Len) /= ".."
                    and then File (File'First .. Name_Len) /= "."
                    and then Is_Directory (Full_Path)
                  then
                     Analyze_Directory
                       (Container_Module & File (File'First .. Name_Len),
                        Full_Path & GNAT.OS_Lib.Directory_Separator);
                  end if;
               end;
            end loop;

            Close (Dir);
         exception
            when GNAT.Directory_Operations.Directory_Error =>
               Warning
                 ("Cannot find the uwrap runtime in path "
                  & To_Wide_Wide_String (Dir_Path));
         end Analyze_Directory;

      begin
         --  Lookup the "stdlib": look into the hardcoded "../include"
         --  directory, used during development. TODO??? we'll probably need
         --  to do something more clever than this at some stage.
         Analyze_Directory
           ("",
            GNATCOLL.Utils.Executable_Location & ".."
            & GNATCOLL.OS.Constants.Dir_Sep & "include"
            & GNATCOLL.OS.Constants.Dir_Sep);

         for Dir_Path of Input_Directories loop
            Analyze_Directory ("", To_String (Dir_Path));
         end loop;

         Load_Module
           (To_String (Args.Template_File.Get),
            Base_Name (To_String (Args.Template_File.Get), ".wrp"));

         Wrapping.Semantic.Analysis.Analyze;
      end;

      --  Step 2, run the template on the selected languages

      declare
         Language : constant String := To_String (Args.Input_Language.Get);
      begin
         if Language = "ada" then
            for Job_Context of Jobs loop
               for Unit of Job_Context.Units_Processed loop
                  Adalang.Input.Analyze_Unit (Unit);
               end loop;
            end loop;
         elsif Language = "json" then
            for File of App.Args.Files.Get loop
               Wrapping.Input.JSON.Analyze_File (To_String (File));
            end loop;
         elsif Language = "test" then
            for File of App.Args.Files.Get loop
               Testlang.Input.Analyze_File (To_String (File));
            end loop;
         else
            Error ("Unknown language '" & To_Text (Language) & "'");
         end if;

         Analyzed_Deferred;
      end;
   end App_Post_Process;

   -------------------------
   -- Dummy_Eval_Property --
   -------------------------

   function Dummy_Get_Property
     (Node : Libtestlang.Analysis.Test_Node;
      Name : Text_Type) return W_Object
   is
      pragma Unreferenced (Name, Node);
   begin
      return null;
   end Dummy_Get_Property;

end Wrapping.Run;
