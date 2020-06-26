with Ada.Text_IO; use Ada.Text_IO;
with Spec_Wrapped; use Spec_Wrapped;

procedure Main is
   V1 : aliased My_1;
   V2 : My_2;
   V3 : My_3 := new My_1;
   V4 : My_4 (10 .. 11);
   V5 : My_5;
   V6 : My_6;
   V7 : My_7;
   V8 : My_8;
begin
   V1 := 2;
   V2 := 9;
   V3.all := 12;
   V4 (10) := V2;
   V5 := B;
   V6.V := V2;
   P (V1);
   V7 := 2;
   V8 := 19;

   Put_Line ("My_1 BOUNDS: " & My_1'First'Img & ", " & My_1'Last'Img);
   Put_Line ("My_2 BOUNDS: " & My_2'First'Img & ", " & My_2'Last'Img);
   Put_Line ("My_7 BOUNDS: " & My_7'First'Img & ", " & My_7'Last'Img);
   Put_Line ("My_8 BOUNDS: " & My_8'First'Img & ", " & My_8'Last'Img);
end Main;
