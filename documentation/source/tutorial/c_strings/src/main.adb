with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with test_Wrapped; use test_Wrapped;

procedure Main is
begin
   s1 ("ADA 1", "ADA 2");
   New_Line;
   Put_Line (s2);
   Put_Line (s3 ("ADA 3", "ADA 4"));
   s4 ("ADA 5", Interfaces.C.Strings.New_String ("ADA 6"));
end Main;
