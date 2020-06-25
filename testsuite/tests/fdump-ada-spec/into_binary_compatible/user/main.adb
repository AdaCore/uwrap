with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;
with System;

with Test_Wrapped; use Test_Wrapped;

procedure Main is
   V : Ptr := Ptr (System.Null_Address);
begin
   V := test_Wrapped.addr (V);
   V := test_Wrapped.addr (V);
end Main;
