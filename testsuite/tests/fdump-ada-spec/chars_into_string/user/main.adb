with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;

with Test_Wrapped; use Test_Wrapped;

procedure Main is
   X : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("S4 C PARAM 2" & ASCII.LF);
begin
   s1 ("S1 PARAM 1" & ASCII.LF, "S1 PARAM 2" & ASCII.LF);
   Put_Line (s2);
   Put_Line (s3 ("S3 PARAM 1" & ASCII.LF,"S3 PARAM 2" & ASCII.LF));
   s4 ("S4 PARAM 1" & ASCII.LF, X);
end Main;
