with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;

with Test_Wrapped; use Test_Wrapped;
with Test_Exceptions; use Test_Exceptions;

with Ada.Exceptions; use Ada.Exceptions;

procedure Main is
begin
   begin
      Ada.Exceptions.Raise_Exception
        (My_Registry.Element (16#02#),
         "Hello");
   end;
exception
   when e2 =>
      Put_Line ("OK");
end Main;
