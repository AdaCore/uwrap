with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;

with Test_Wrapped; use Test_Wrapped;

procedure Main is
   V : Int_Ptr_Array (1 .. 10);
begin
   for I in V'Range loop
      V (I) := new Integer'(Integer (I) * 10);
   end loop;
   Test_Wrapped.Read (V);
end Main;
