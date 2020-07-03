with Ada.Text_IO; use Ada.Text_IO;
package body Other_Spec is

   procedure P (V : X) is
   begin
      Put_Line ("OTHER SPEC =" & V'Img);
   end P;

end Other_Spec;
