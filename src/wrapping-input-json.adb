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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO;    use Ada.Wide_Wide_Text_IO;
with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;

with GNATCOLL.Strings_Impl; use GNATCOLL.Strings_Impl;
with GNATCOLL.Mmap;         use GNATCOLL.Mmap;

with Wrapping.Runtime.Commands; use Wrapping.Runtime.Commands;
with Wrapping.Runtime.Strings;  use Wrapping.Runtime.Strings;
with Wrapping.Runtime.Frames;   use Wrapping.Runtime.Frames;

package body Wrapping.Input.JSON is

   ----------------
   -- Push_Value --
   ----------------

   overriding function Push_Value
     (An_Entity : access W_JSON_Node_Type; Name : Text_Type) return Boolean
   is
   begin
      if W_Node_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      if Name = "name" then
         Push_Object
           (W_Object'
              (new W_String_Type'(Value => An_Entity.Name, others => <>)));

         return True;
      elsif Name = "kind" then
         declare
            Kind : Wide_Wide_String := An_Entity.Node.Kind'Wide_Wide_Image;
            Kind_Text : Wide_Wide_String :=
              To_Lower (Kind (Kind'First + 5 .. Kind'Last - 5));
         begin
            Push_Object
              (W_Object'
                 (new W_String_Type'
                    (Value => To_Unbounded_Text (Kind_Text), others => <>)));
         end;

         return True;
      elsif Name = "value" then
         case An_Entity.Node.Kind is
            when JSON_Null_Type =>
               Error ("unimplemented");

            when JSON_Boolean_Type =>
               Error ("unimplemented");

            when JSON_Int_Type =>
               Push_Object
                 (W_Object'
                    (new W_Integer_Type'
                       (Value => An_Entity.Node.Get, others => <>)));

            when JSON_Float_Type =>
               Error ("unimplemented");

            when JSON_String_Type =>
               Push_Object (To_W_String (Decode (An_Entity.Node.Get, UTF_8)));

            when JSON_Array_Type =>
               Error ("unimplemented");

            when JSON_Object_Type =>
               Error ("unimplemented");

         end case;

         return True;
      end if;

      return False;
   end Push_Value;

   ------------------
   -- Write_String --
   ------------------

   overriding function Write_String
     (Object : W_JSON_Node_Type) return Buffer_Slice
   is
   begin
      return Get_Empty_Slice;
   end Write_String;

   ---------------------
   -- To_Debug_String --
   ---------------------

   overriding function To_Debug_String
     (Object : W_JSON_Node_Type) return Text_Type
   is
   begin
      return "";
   end To_Debug_String;

   ------------------
   -- Analyze_File --
   ------------------

   procedure Analyze_File (Filename : String) is
      File : Mapped_File;
      Str  : Str_Access;
      Root : JSON_Value;

      Root_Node    : W_JSON_Node;
      Current_Node : W_JSON_Node := new W_JSON_Node_Type;

      ----------------------
      -- Iterate_On_Value --
      ----------------------

      procedure Iterate_On_Value (Name : UTF8_String; Value : JSON_Value) is
         Parent : W_JSON_Node := Current_Node;
      begin
         Current_Node      := new W_JSON_Node_Type;
         Current_Node.Node := Value;
         Current_Node.Name := To_Unbounded_Text (Decode (Name, UTF_8));
         Add_Child (Parent, Current_Node);

         if Value.Kind = JSON_Object_Type then
            Map_JSON_Object (Value, Iterate_On_Value'Access);
         elsif Value.Kind = JSON_Array_Type then
            for O of JSON_Array'(Value.Get) loop
               Iterate_On_Value ("", O);
            end loop;
         end if;

         Current_Node := Parent;
      end Iterate_On_Value;

   begin
      File := Open_Read (Filename);
      Read (File);
      Str := Data (File);

      Root := Read (Str.all (1 .. Last (File)), Filename);

      Current_Node.Node := Root;
      Root_Node         := Current_Node;

      Map_JSON_Object (Root, Iterate_On_Value'Access);

      Close (File);

      Analyse_Input (W_Node (Root_Node));
   end Analyze_File;

end Wrapping.Input.JSON;
