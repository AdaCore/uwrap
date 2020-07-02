with Ada.Text_IO; use Ada.Text_IO;
with Spec_Wrapped; use Spec_Wrapped;

procedure Main is
   A : My_T1;
   B : My_T2;
begin
   Spec_Wrapped.My_V (A, B);
end Main;
