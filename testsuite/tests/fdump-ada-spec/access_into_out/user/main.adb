with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;

with Test_Wrapped; use Test_Wrapped;

procedure Main is
   A, B, C : aliased My_Struct;
begin
   XGet1 (A, B, C);
   Put_Line ("XGet1" & A.F'Img & B.F'Img & C.F'Img);
   XGet2 ((F=> 99), A);
   Put_Line ("XGet2" & A.F'Img);
   XGet3 ((F=> 98));
   XSet (A'Access);
   Put_Line ("XGet3" & A.F'Img);
end Main;
