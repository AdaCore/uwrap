with Ada.Containers.Vectors;
use Ada.Containers;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package body Wrapping is

   type Error_Location is record
      Filename : Unbounded_String;
      Loc : Source_Location;
   end record;

   package Error_Location_Vector is new Ada.Containers.Vectors (Positive, Error_Location);

   Error_Stack : Error_Location_Vector.Vector;

   function Get_Sloc_Str return String is
   begin
       if Error_Stack.Length > 0 then
         return
           To_String (Error_Stack.Last_Element.Filename)
           & ":" & Trim (Error_Stack.Last_Element.Loc.Line'Image, Left)
           & ":" & Trim (Error_Stack.Last_Element.Loc.Column'Image, Left);
      else
         return "";
      end if;
   end Get_Sloc_Str;

   Error_Message : Unbounded_Text_Type;

   procedure Error (Message : Text_Type) is
   begin
      Error_Message := To_Unbounded_Text (Message);

      if Error_Callback /= null then
         Error_Callback.all
           (Message,
            To_String (Error_Stack.Last_Element.Filename),
            Error_Stack.Last_Element.Loc);
      else
         if Error_Stack.Length > 0 then
            Put_Line
              (To_Text (Get_Sloc_Str)
               & ": " & Message);
         else
            Put_Line (Message);
         end if;

         raise Wrapping_Error;
      end if;
   end Error;

   procedure Push_Error_Location (Filename : String; Loc : Source_Location) is
   begin
      Error_Stack.Append
        (Error_Location'
           (To_Unbounded_String (Ada.Directories.Simple_Name
            (Filename)), Loc));
   end Push_Error_Location;

   procedure Pop_Error_Location is
   begin
      Error_Stack.Delete_Last;
   end Pop_Error_Location;

end Wrapping;
