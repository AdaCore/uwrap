with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with GNATCOLL.OS.Constants; use GNATCOLL.OS.Constants;
with GNATCOLL.Utils; use GNATCOLL.Utils;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libtestlang.Analysis;
with Libtestlang.Common;
with Libtestlang.Introspection;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Introspection;

with Wrapping.Semantic; use Wrapping.Semantic;
with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Wrapping.Input.Kit;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;

package body Wrapping.Run is

   package Testlang is
      use Libtestlang.Analysis;
      use Libtestlang.Common;
      use Libtestlang.Introspection;

      package Input is new Wrapping.Input.Kit
        (Kit_Node                   => Libtestlang.Analysis.Test_Node,
         Kit_Unit                   => Libtestlang.Analysis.Analysis_Unit,
         Kit_Node_Array             => Libtestlang.Analysis.Test_Node_Array,
         Any_Node_Data_Reference    => Libtestlang.Common.Any_Node_Data_Reference,
         Any_Node_Type_Id           => Libtestlang.Common.Any_Node_Type_Id,
         Kit_Node_Kind_Type         => Libtestlang.Common.Test_Node_Kind_Type,
         Analysis_Unit              => Libtestlang.Analysis.Analysis_Unit,
         Analysis_Context           => Libtestlang.Analysis.Analysis_Context,
         Grammar_Rule               => Libtestlang.Common.Grammar_Rule,
         Unit_Provider_Reference    => Libtestlang.Analysis.Unit_Provider_Reference,
         None                       => Libtestlang.Common.None,
         No_Kit_Node                => Libtestlang.Analysis.No_Test_Node,
         Default_Grammar_Rule       => Libtestlang.Common.Default_Grammar_Rule,
         Default_Charset            => Libtestlang.Common.Default_Charset,
         No_Unit_Provider_Reference => Libtestlang.Analysis.No_Unit_Provider_Reference,
         No_Node_Type_Id            => Libtestlang.Common.None,
         Any_Value_Kind             => Libtestlang.Common.Any_Value_Kind,
         Value_Type                 => Libtestlang.Introspection.Value_Type,
         Value_Array                => Libtestlang.Introspection.Value_Array,
         Root                       => Libtestlang.Analysis.Root,
         Text_Type_Value            => Libtestlang.Common.Text_Type_Value,
         Node_Value                 => Libtestlang.Common.Node_Value,
         Value_Constraint           => Libtestlang.Common.Value_Constraint,
         Value_Constraint_Array     => Libtestlang.Common.Value_Constraint_Array);
   end Testlang;

   package Adalang is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use Libadalang.Introspection;

      package Input is new Wrapping.Input.Kit
        (Kit_Node                   => Libadalang.Analysis.Ada_Node,
         Kit_Unit                   => Libadalang.Analysis.Analysis_Unit,
         Kit_Node_Array             => Libadalang.Analysis.Ada_Node_Array,
         Any_Node_Data_Reference    => Libadalang.Common.Any_Node_Data_Reference,
         Any_Node_Type_Id           => Libadalang.Common.Any_Node_Type_Id,
         Kit_Node_Kind_Type         => Libadalang.Common.Ada_Node_Kind_Type,
         Analysis_Unit              => Libadalang.Analysis.Analysis_Unit,
         Analysis_Context           => Libadalang.Analysis.Analysis_Context,
         Grammar_Rule               => Libadalang.Common.Grammar_Rule,
         Unit_Provider_Reference    => Libadalang.Analysis.Unit_Provider_Reference,
         None                       => Libadalang.Common.None,
         No_Kit_Node                => Libadalang.Analysis.No_Ada_Node,
         Default_Grammar_Rule       => Libadalang.Common.Default_Grammar_Rule,
         Default_Charset            => Libadalang.Common.Default_Charset,
         No_Unit_Provider_Reference => Libadalang.Analysis.No_Unit_Provider_Reference,
         No_Node_Type_Id            => Libadalang.Common.None,
         Any_Value_Kind             => Libadalang.Common.Any_Value_Kind,
         Value_Type                 => Libadalang.Introspection.Value_Type,
         Value_Array                => Libadalang.Introspection.Value_Array,
         Root                       => Libadalang.Analysis.Root,
         Text_Type_Value            => Libadalang.Common.Text_Type_Value,
         Node_Value                 => Libadalang.Common.Node_Value,
         Value_Constraint           => Libadalang.Common.Value_Constraint,
         Value_Constraint_Array     => Libadalang.Common.Value_Constraint_Array);
   end Adalang;

   procedure App_Post_Process
     (Context : Libadalang.Helpers.App_Context;
      Jobs    : Libadalang.Helpers.App_Job_Context_Array)
   is
   begin
      -- Step 1, load the templates

      declare
         Input_Directories : Args.Input_Directories.Result_Array :=
           Args.Input_Directories.Get;

         procedure Analyze_Directory
           (Base_Module_Name : String;
            Dir_Path         : String)
         is
            Dir : Dir_Type;
            File : String (1 .. 2048);
            Name_Len : Integer;

            Container_Module : String :=
              (if Base_Module_Name /= "" then Base_Module_Name & "." else "");
         begin
            Open (Dir, Dir_Path);

            loop
               Read (Dir, File, Name_Len);

               exit when Name_Len = 0;

               declare
                  Full_Path : String := Dir_Path & File (File'First .. Name_Len);
               begin

                  if File_Extension (File (File'First .. Name_Len)) = ".wrp" then
                     declare
                        Module_Name : String :=
                          Container_Module & Base_Name (File (File'First .. Name_Len), ".wrp");
                     begin
                        Load_Module (Full_Path, Module_Name);
                     end;
                  elsif File (File'First .. Name_Len) /= ".."
                    and then File (File'First .. Name_Len) /= "."
                    and then Is_Directory (Full_Path)
                  then
                     Analyze_Directory
                       (Container_Module
                        & File (File'First .. Name_Len),
                        Full_Path & GNAT.OS_Lib.Directory_Separator);
                  end if;
               end;
            end loop;

            Close (Dir);
         end;

      begin
         Analyze_Directory
           ("",
            GNATCOLL.Utils.Executable_Location
            & ".." & GNATCOLL.OS.Constants.Dir_Sep
            & "include" & GNATCOLL.OS.Constants.Dir_Sep);

         for Dir_Path of Input_Directories loop
            Analyze_Directory ("", To_String (Dir_Path));
         end loop;

         Load_Module
           (To_String (Args.Template_File.Get), Base_Name (To_String (Args.Template_File.Get), ".wrp"));

         Wrapping.Semantic.Analysis.Analyze;
      end;

      -- Step 2, run the template on the selected languages

      declare
         Language : String := To_String (Args.Input_Language.Get);
      begin
         if Language = "ada" then
            for Job_Context of Jobs loop
               for Unit of Job_Context.Units_Processed loop
                  Adalang.Input.Analyze_Unit (Unit);
               end loop;
            end loop;

            Analyze_Templates;
         elsif Language = "proxy" then
            null;
         elsif Language = "test" then
            for File of App.Args.Files.Get loop
               Testlang.Input.Analyze_File (To_String (File));
            end loop;
         else
            Error ("Unknown language '" & To_Text (Language) & "'");
         end if;
      end;
   end App_Post_Process;

end Wrapping.Run;
