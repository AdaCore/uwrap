with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;
with System;

with Test_Wrapped; use Test_Wrapped;

procedure Main is
   X : test_Wrapped.Integer_Array := (10, 11, 12);
begin
   test_Wrapped.changeArray (X);

   for I of X loop
      Put_Line ("VALUE Ada =" & I'Img);
   end loop;
end Main;
