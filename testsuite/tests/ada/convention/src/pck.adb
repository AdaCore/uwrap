with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   procedure P (V1 : A_Type1; V2 : A_Type2) is
   begin
      Put_Line (V1'Img);
      Put_Line (V2'Img);
   end P;

   procedure P2 (X : R) is
   begin
      Put_Line
        (X.V'Img & ", "
         & X.V2'Img & ", "
         & X.B1'Img & ", "
         & X.B2'Img & ", "
         & X.I1'Img & ", "
         & X.B3'Img);
   end P2;

end Pck;
