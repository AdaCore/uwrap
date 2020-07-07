with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;

with Test_Wrapped; use Test_Wrapped;

procedure Main is
   A, B, C : aliased My_Struct := (F => 123);
begin
   A := XGet1;
   Put_Line (A.F'Img);
end Main;
