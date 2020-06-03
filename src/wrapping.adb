with Ada.Containers.Vectors;
use Ada.Containers;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package body Wrapping is

   type Error_Location is record
      Filename : Unbounded_Text_Type;
      Loc : Source_Location;
   end record;

   package Error_Location_Vector is new Ada.Containers.Vectors (Positive, Error_Location);

   Error_Stack : Error_Location_Vector.Vector;

   procedure Error (Message : Text_Type) is
   begin
      if Error_Stack.Length > 0 then
         Put_Line
           (To_Text (Error_Stack.Last_Element.Filename)
            & ":" & Trim (Error_Stack.Last_Element.Loc.Line'Wide_Wide_Image, Left)
            & ":" & Trim (Error_Stack.Last_Element.Loc.Column'Wide_Wide_Image, Left)
            & ": " & Message);
      else
         Put_Line (Message);
      end if;

      raise Wrapping_Error;
   end Error;

   procedure Push_Error_Location (Filename : String; Loc : Source_Location) is
   begin
      Error_Stack.Append (Error_Location'(To_Unbounded_Text (To_Wide_Wide_String (Filename)), Loc));
   end Push_Error_Location;

   procedure Pop_Error_Location is
   begin
      Error_Stack.Delete_Last;
   end Pop_Error_Location;

end Wrapping;
