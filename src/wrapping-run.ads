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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse;
with Langkit_Support.Token_Data_Handlers;

with Libadalang.Helpers; use Libadalang.Helpers;
with Libtestlang.Analysis;
with Libtestlang.Common;
with Libtestlang.Introspection;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Introspection;

with Wrapping.Input.Kit;
with Wrapping.Input.Ada;         use Wrapping.Input.Ada;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;

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

   function Dummy_Get_Property
     (Node : Libtestlang.Analysis.Test_Node;
      Name : Text_Type) return W_Object;

   package Testlang is
      use Libtestlang.Analysis;
      use Libtestlang.Common;
      use Libtestlang.Introspection;

      package Input is new Wrapping.Input.Kit
        (Language_Name              => "test",
         Kit_Node                   => Libtestlang.Analysis.Test_Node,
         Kit_Unit                   => Libtestlang.Analysis.Analysis_Unit,
         Kit_Node_Array             => Libtestlang.Analysis.Test_Node_Array,
         Any_Node_Data_Reference    =>
            Libtestlang.Common.Any_Node_Data_Reference,
         Any_Node_Type_Id           => Libtestlang.Common.Any_Node_Type_Id,
         Kit_Node_Kind_Type         => Libtestlang.Common.Test_Node_Kind_Type,
         Analysis_Unit              => Libtestlang.Analysis.Analysis_Unit,
         Analysis_Context           => Libtestlang.Analysis.Analysis_Context,
         Grammar_Rule               => Libtestlang.Common.Grammar_Rule,
         Unit_Provider_Reference    =>
            Libtestlang.Analysis.Unit_Provider_Reference,
         Token_Reference            => Libtestlang.Common.Token_Reference,
         Token_Data_Type            => Libtestlang.Common.Token_Data_Type,
         Token_Kind                 => Libtestlang.Common.Token_Kind,
         Token_Index                =>
            Langkit_Support.Token_Data_Handlers.Token_Index,
         None                       => Libtestlang.Common.None,
         Default_Grammar_Rule       => Libtestlang.Common.Default_Grammar_Rule,
         Default_Charset            => Libtestlang.Common.Default_Charset,
         No_Unit_Provider_Reference =>
            Libtestlang.Analysis.No_Unit_Provider_Reference,
         No_Node_Type_Id            => Libtestlang.Common.None,
         Root                       => Libtestlang.Analysis.Root,
         No_Token                   => Libtestlang.Common.No_Token,
         Get_Property               => Dummy_Get_Property);
   end Testlang;

   package Adalang is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use Libadalang.Introspection;

      package Input is new Wrapping.Input.Kit
        (Language_Name              => "ada",
         Kit_Node                   => Libadalang.Analysis.Ada_Node,
         Kit_Unit                   => Libadalang.Analysis.Analysis_Unit,
         Kit_Node_Array             => Libadalang.Analysis.Ada_Node_Array,
         Any_Node_Data_Reference    =>
            Libadalang.Common.Any_Node_Data_Reference,
         Any_Node_Type_Id           => Libadalang.Common.Any_Node_Type_Id,
         Kit_Node_Kind_Type         => Libadalang.Common.Ada_Node_Kind_Type,
         Analysis_Unit              => Libadalang.Analysis.Analysis_Unit,
         Analysis_Context           => Libadalang.Analysis.Analysis_Context,
         Grammar_Rule               => Libadalang.Common.Grammar_Rule,
         Unit_Provider_Reference    =>
            Libadalang.Analysis.Unit_Provider_Reference,
         Token_Reference            => Libadalang.Common.Token_Reference,
         Token_Data_Type            => Libadalang.Common.Token_Data_Type,
         Token_Kind                 => Libadalang.Common.Token_Kind,
         Token_Index                =>
            Langkit_Support.Token_Data_Handlers.Token_Index,
         None                       => Libadalang.Common.None,
         Default_Grammar_Rule       => Libadalang.Common.Default_Grammar_Rule,
         Default_Charset            => Libadalang.Common.Default_Charset,
         No_Unit_Provider_Reference =>
            Libadalang.Analysis.No_Unit_Provider_Reference,
         No_Node_Type_Id            => Libadalang.Common.None,
         Root                       => Libadalang.Analysis.Root,
         No_Token                   => Libadalang.Common.No_Token,
         Get_Property               => Wrapping.Input.Ada.Get_Property);
   end Adalang;

end Wrapping.Run;
