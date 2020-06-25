with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;

with Test_Wrapped; use Test_Wrapped;
with Test_Exceptions; use Test_Exceptions;

with Ada.Exceptions; use Ada.Exceptions;

procedure Main is
begin
   begin
      test_Wrapped.someFunction;
   exception
      when e1 =>
         Put_Line ("E1");
   end;

   begin
      Ada.Exceptions.Raise_Exception
        (My_Registry.Element (Integer (test_Wrapped.lastError)),
         "Hello");
   exception
      when e2 =>
         Put_Line ("E2");
   end;

   begin
      test_Wrapped.someOtherFunction;
   exception
      when e3 =>
         Put_Line ("E3");
   end;
end Main;
