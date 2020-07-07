with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;

with Test_Wrapped; use Test_Wrapped;
with Test_Exceptions; use Test_Exceptions;

with Ada.Exceptions; use Ada.Exceptions;

procedure Main is
   S : My_Struct;
begin
   S := Test_Wrapped.Some_Function (0);

   Put_Line (S.F'Img);

   begin
      S := Test_Wrapped.Some_Function (1);
   exception
      when e1 =>
         Put_Line ("E1");
   end;

   S := Test_Wrapped.Some_Other_Function (0);

   Put_Line (S.F'Img);

   begin
      S := Test_Wrapped.Some_Other_Function (3);
   exception
      when e3 =>
         Put_Line ("E3");
   end;
end Main;
