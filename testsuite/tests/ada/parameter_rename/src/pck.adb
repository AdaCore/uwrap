with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   procedure Proc (P1 : Integer; P2 : Float)
   is
   begin
      Put_Line (P1'Img & " - " & P2'Img);
   end Proc;

end Pck;
