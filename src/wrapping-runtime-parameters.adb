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

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Wrapping.Runtime.Frames;    use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Commands;  use Wrapping.Runtime.Commands;

package body Wrapping.Runtime.Parameters is

   --------------------
   -- Make_Parameter --
   --------------------

   function Make_Parameter
     (Name : Text_Type; Is_Optional : Boolean) return Parameter
   is
   begin
      return
        Parameter'
          (Name => To_Unbounded_Text (Name), Is_Optional => Is_Optional);
   end Make_Parameter;

   ------------------------
   -- Process_Parameters --
   ------------------------

   function Process_Parameters
     (Profile : Parameter_Profile; Arg : T_Arg_Vectors.Vector)
      return Actuals_Type
   is
      Result : Actuals_Type (Profile'Range) := (others => null);

      Parameter_Index  : Integer;
      In_Named_Section : Boolean := False;
      Formal           : Parameter;
      Param            : T_Arg;
   begin
      Parameter_Index := 1;

      for Actual_Index in 1 .. Arg.Length loop
         Param := Arg.Element (Integer (Actual_Index));

         if Param.Name /= "" then
            In_Named_Section := True;

            declare
               Name  : Text_Type := To_Text (Param.Name);
               Found : Boolean   := False;
            begin
               for I in Profile'Range loop
                  if Profile (I).Name = Name then
                     Parameter_Index := I;
                     Formal          := Profile (Parameter_Index);
                     Found           := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Error ("parameter name '" & Name & "' doesn't exit");
               end if;
            end;
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;

            Formal := Profile (Parameter_Index);
         end if;

         Result (Parameter_Index) := Param.Expr;

         Parameter_Index := Parameter_Index + 1;
      end loop;

      return Result;
   end Process_Parameters;

   ----------------------------
   -- Handle_Call_Parameters --
   ----------------------------

   procedure Handle_Call_Parameters
     (Args               : T_Arg_Vectors.Vector;
      Evaluate_Parameter : access procedure
        (Name : Text_Type; Position : Integer; Value : T_Expr))
   is
      Parameter_Index  : Integer;
      In_Named_Section : Boolean := False;
   begin
      Push_Frame_Context_Parameter;

      Parameter_Index := 1;

      for Param of Args loop
         Push_Error_Location (Param.Node);

         if Param.Name /= "" then
            In_Named_Section := True;
            Evaluate_Parameter
              (Param.Name_Node.Text, Parameter_Index, Param.Expr);
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;

            Evaluate_Parameter ("", Parameter_Index, Param.Expr);
         end if;

         Pop_Error_Location;
         Parameter_Index := Parameter_Index + 1;
      end loop;

      Pop_Frame_Context;
   end Handle_Call_Parameters;

end Wrapping.Runtime.Parameters;
