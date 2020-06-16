with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package Wrapping is

   Wrapping_Error : exception;

   procedure Error (Message : Text_Type);

   procedure Push_Error_Location (Filename : String; Loc : Source_Location);

   procedure Pop_Error_Location;

   function Get_Sloc_Str return String;

end Wrapping;
