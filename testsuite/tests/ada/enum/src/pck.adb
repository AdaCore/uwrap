with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   procedure P (V1 : A_Type1; V2 : A_Type2) is
   begin
      Put_Line (V1'Img);
      Put_Line (V2'Img);
   end P;

end Pck;
