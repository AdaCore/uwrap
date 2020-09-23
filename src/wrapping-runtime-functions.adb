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

with Ada.Containers;                    use Ada.Containers;
with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Conversions;        use Ada.Characters.Conversions;

with Wrapping.Utils;               use Wrapping.Utils;
with Wrapping.Runtime.Commands;    use Wrapping.Runtime.Commands;
with Wrapping.Runtime.Strings;     use Wrapping.Runtime.Strings;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;
with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Matching;    use Wrapping.Runtime.Matching;
with Wrapping.Runtime.Parameters;  use Wrapping.Runtime.Parameters;

package body Wrapping.Runtime.Functions is

   -----------------------------
   -- Call_Normalize_Ada_Name --
   -----------------------------

   P_Normalize_Ada_Name : Parameter_Profile :=
     (1 => Make_Parameter ("str", False));

   procedure Call_Normalize_Ada_Name
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      New_Name : Unbounded_Text_Type;
      Prev_Up  : Boolean := False;
      Prev_Sep : Boolean := False;

      Actuals : Actuals_Type :=
        Process_Parameters (P_Normalize_Ada_Name, Params);
   begin
      Push_Buffer_Cursor;

      declare
         Slice : Buffer_Slice :=
           Evaluate_Expression (Actuals (1)).Write_String;
         Name : Text_Type := Copy_String (Slice);
         C : Integer := Name'First;
      begin
         while C <= Name'Last loop
            if Is_Upper (Name (C)) then
               if not Prev_Up and then not Prev_Sep
                 and then Length (New_Name) > 0
               then
                  Append (New_Name, "_");
                  Prev_Sep := True;
               end if;

               Prev_Up := True;
            else
               Prev_Up := False;
            end if;

            if Length (New_Name) = 0 or else Prev_Sep then
               Append (New_Name, To_Upper (Name (C)));
            else
               Append (New_Name, Name (C));
            end if;

            if Name (C) = '_' or else Name (C) = '.' then
               Prev_Sep := True;
            else
               Prev_Sep := False;
            end if;

            C := C + 1;
         end loop;
      end;

      Pop_Buffer_Cursor;
      Push_Object (To_W_String (New_Name));
   end Call_Normalize_Ada_Name;

   -----------------------
   -- Call_Replace_Text --
   -----------------------

   P_Replace_Text : Parameter_Profile :=
     (Make_Parameter ("source", False), Make_Parameter ("pattern", False),
      Make_Parameter ("replace", False));

   procedure Call_Replace_Text
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Result  : W_Object;
      Actuals : Actuals_Type := Process_Parameters (P_Replace_Text, Params);
   begin
      Push_Buffer_Cursor;

      declare
         Source_Slice : Buffer_Slice :=
           Evaluate_Expression (Actuals (1)).Write_String;
         Pattern_Slice : Buffer_Slice :=
           Evaluate_Expression (Actuals (2)).Write_String;
         Replace_Slice : Buffer_Slice :=
           Evaluate_Expression (Actuals (3)).Write_String;
      begin
         Result :=
           W_Object
             (To_W_String
                (Replace_String (
                 Buffer.Str
                   (Source_Slice.First.Offset .. Source_Slice.Last.Offset),
                  Buffer.Str
                   (Pattern_Slice.First.Offset .. Pattern_Slice.Last.Offset),
                  Buffer.Str
                   (Replace_Slice.First.Offset
                    .. Replace_Slice.Last.Offset))));
      end;

      Pop_Buffer_Cursor;
      Push_Object (Result);
   end Call_Replace_Text;

   -------------------
   -- Call_To_Lower --
   -------------------

   P_To_Lower : Parameter_Profile := (1 => Make_Parameter ("str", False));

   procedure Call_To_Lower
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Result : W_Object;

      Actuals : Actuals_Type := Process_Parameters (P_To_Lower, Params);
      Slice : Buffer_Slice;
   begin
      Push_Buffer_Cursor;
      Slice := Evaluate_Expression (Actuals (1)).Write_String;
      Result :=
        W_Object
          (To_W_String
             (To_Lower
                (Buffer.Str
                     (Slice.First.Offset .. Slice.Last.Offset))));
      Pop_Buffer_Cursor;
      Push_Object (Result);
   end Call_To_Lower;

   -------------------
   -- Call_Reindent --
   -------------------

   P_Unindent : Parameter_Profile :=
     (Make_Parameter ("ident", False), Make_Parameter ("str", False));

   procedure Call_Reindent
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Result : W_Object;

      Actuals     : Actuals_Type := Process_Parameters (P_Unindent, Params);
      Indentation : W_Object;
      Slice : Buffer_Slice;
   begin
      Indentation := Evaluate_Expression (Actuals (1)).Dereference;

      if Indentation.all not in W_Integer_Type'Class then
         Error ("expected integer object");
      end if;

      Push_Buffer_Cursor;
      Slice := Evaluate_Expression (Actuals (2)).Write_String;
      Pop_Buffer_Cursor;

      Result :=
        W_Object
          (To_W_String
             (Reindent
                (W_Integer (Indentation).Value,
                 Buffer.Str (Slice.First.Offset .. Slice.Last.Offset),
                 True)));
      Push_Object (Result);
   end Call_Reindent;

   ------------------
   -- Call_Max_Col --
   ------------------

   P_Max_Col : Parameter_Profile :=
     (1 => Make_Parameter ("str", False));

   procedure Call_Max_Col
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Result  : W_Object;
      Actuals : Actuals_Type := Process_Parameters (P_Max_Col, Params);
      Obj     : W_Object := Evaluate_Expression (Actuals (1));
      Dummy   : Buffer_Slice;
   begin
      Push_Buffer_Cursor;
      Reset_Max_Column;
      Dummy := Obj.Write_String;
      Result := new W_Integer_Type'(Value => Buffer.Cursor.Max_Column);
      Pop_Buffer_Cursor;

      Push_Object (Result);
   end Call_Max_Col;

   --------------------------
   -- Call_Convert_To_Text --
   --------------------------

   procedure Call_Convert_To_Text
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
   begin
      if Params.Length in 1 .. 2 then
         Push_Frame_Context_Parameter;

         Push_Object
           (W_Object'
              (new W_Text_Conversion_Type'
                 (An_Object =>
                    Evaluate_Expression (Params.Element (1).Expr))));

         Pop_Frame_Context;

         if Params.Length = 2 then
            Push_Match_Result (Params.Element (2).Expr);
            Pop_Underneath_Top;
         end if;
      else
         Error ("conversion takes up to 2 arguments");
      end if;
   end Call_Convert_To_Text;

   ----------------------------
   -- Call_Convert_To_String --
   ----------------------------

   procedure Call_Convert_To_String
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Slice : Buffer_Slice;
   begin
      if Params.Length in 1 .. 2 then
         Push_Frame_Context_Parameter;
         Push_Buffer_Cursor;
         Slice :=
           Evaluate_Expression
             (Params.Element (1).Expr).Write_String;
         Pop_Buffer_Cursor;

         Push_Object
           (To_W_String
              (Buffer.Str
                   (Slice.First.Offset .. Slice.Last.Offset)));

         if Params.Length = 2 then
            Push_Match_Result (Params.Element (2).Expr);
            Pop_Underneath_Top;
         end if;
      else
         Error ("conversion takes up to 2 arguments");
      end if;
   end Call_Convert_To_String;

end Wrapping.Runtime.Functions;
