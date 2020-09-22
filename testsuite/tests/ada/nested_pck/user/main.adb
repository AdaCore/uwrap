with Ada.Text_IO; use Ada.Text_IO;
with Spec_Wrapped; use Spec_Wrapped;

procedure Main is
begin
   Spec_Wrapped.A;
   Spec_Wrapped.Nested.B;
end Main;
