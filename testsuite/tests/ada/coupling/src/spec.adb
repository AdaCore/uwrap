with Ada.Text_IO; use Ada.Text_IO;

package body Spec is

   procedure P (V : X) is
   begin
      Put_Line ("SPEC =" & V'Img);
   end P;

end Spec;
