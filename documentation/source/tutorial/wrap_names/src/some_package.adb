with Ada.Text_IO; use Ada.Text_IO;

package body Some_Package is

   procedure Some_Proc (Some_Value_1 : Some_Type; Some_Value_2 : Integer) is
   begin
      Put_Line (Some_Value_1'Img & ", " & Some_Value_2'Img);
   end Some_Proc;

end Some_Package;
