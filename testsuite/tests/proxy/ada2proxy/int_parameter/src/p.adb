with Ada.Text_IO; use Ada.Text_IO;

package body P is

   procedure Proc (V : Integer) is
   begin
      Put_Line ("Received: " & V'Img);
   end Proc;

end P;
