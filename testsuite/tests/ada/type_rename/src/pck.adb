with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   procedure Proc (P1 : A_Type; P2 : A_Record)
   is
   begin
      Put_Line (P1'Img);
      Put_Line (P2.A'Img & "," & P2.B'Img & "," & P2.C'Img);
   end Proc;

end Pck;
