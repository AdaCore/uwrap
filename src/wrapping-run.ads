with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse;

with Libadalang.Helpers; use Libadalang.Helpers;

package Wrapping.Run is

   procedure App_Post_Process
     (Context : Libadalang.Helpers.App_Context;
      Jobs    : Libadalang.Helpers.App_Job_Context_Array);

   package App is new Libadalang.Helpers.App
     ("uwrap", "Universal Code Wrapper",
      App_Post_Process => App_Post_Process);

   package Args is
      use GNATCOLL.Opt_Parse;

      package Input_Language is new Parse_Option
        (App.Args.Parser, "-l", "--input-language",
         "Input Language, either ada or proxy",
         Unbounded_String, Default_Val => Null_Unbounded_String);

      package Template_File is new Parse_Option
        (App.Args.Parser, "-w", "--wrapper-template",
         "Use this template wrapper file",
         Unbounded_String, Default_Val => Null_Unbounded_String);

      package Input_Directories is new Parse_Option_List
        (App.Args.Parser, "-I", "",
         "Input file directory",
         Arg_Type => Unbounded_String,
         Accumulate => True);

   end Args;

end Wrapping.Run;
