with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Libtestlang.Analysis;

with Wrapping.Semantic; use Wrapping.Semantic;
with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Wrapping.Input.Ada; use Wrapping.Input.Ada;
with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Wrapping.Input.Kit; use Wrapping.Input.Kit;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Ada.Text_IO; use Ada.Text_IO;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

package body Wrapping.Run is

   procedure App_Post_Process
     (Context : Libadalang.Helpers.App_Context;
      Jobs    : Libadalang.Helpers.App_Job_Context_Array)
   is
   begin
      -- Step 1, load the templates

      declare
         Input_Directories : Args.Input_Directories.Result_Array :=
           Args.Input_Directories.Get;

         Dir : Dir_Type;
         File : String (1 .. 2048);
         Name_Len : Integer;
      begin
         for Dir_Name of Input_Directories loop
            Open (Dir, To_String (Dir_Name));

            loop
               Read (Dir, File, Name_Len);

               exit when Name_Len = 0;

               if File_Extension (File (File'First .. Name_Len)) = ".wrp" then
                  Wrapping.Semantic.Analysis.Files.Append
                    (To_String (Dir_Name) & File (File'First .. Name_Len));
               end if;
            end loop;
         end loop;

         Wrapping.Semantic.Analysis.Files.Append
           (To_String (Args.Template_File.Get));

         Wrapping.Semantic.Analysis.Analyze;
      end;

      -- Step 2, run the template on the selected languages

      declare
         Language : String := To_String (Args.Input_Language.Get);
      begin
         if Language = "ada" then
            Wrapping.Input.Ada.Register_Globals;

            for Job_Context of Jobs loop
               for Unit of Job_Context.Units_Processed loop
                  Wrapping.Input.Ada.Populate_Language_Entities
                    (Unit.Root);
               end loop;
            end loop;

             Wrapping.Runtime.Analysis.Analyse (To_Text (Language));
         elsif Language = "proxy" then
            null;
         elsif Language = "test" then
            declare
               Unit : Libtestlang.Analysis.Analysis_Unit;
               Context : Libtestlang.Analysis.Analysis_Context := Libtestlang.Analysis.Create_Context;
               Root_Entity : Language_Entity;

            begin
               for File of App.Args.Files.Get loop
                  Unit := Libtestlang.Analysis.Get_From_File (Context, To_String (File));

                  if Libtestlang.Analysis.Has_Diagnostics (Unit) then
                     for D of Libtestlang.Analysis.Diagnostics (Unit) loop
                        Put_Line (To_String (File) & ":" & To_Pretty_String (D));
                     end loop;
                  end if;

                  Root_Entity := new Kit_Language_Entity_Type'(Node => Unit.Root, others => <>);

                  Wrapping.Runtime.analysis.Analyse (Root_Entity);
               end loop;
            end;
         else
            Error ("Unknown language '" & To_Text (Language) & "'");
         end if;


      end;
   end App_Post_Process;

end Wrapping.Run;
