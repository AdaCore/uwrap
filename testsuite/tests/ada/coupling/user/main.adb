with Ada.Text_IO; use Ada.Text_IO;
with Spec_Wrapped; use Spec_Wrapped;
with Other_Spec_Wrapped; use Other_Spec_Wrapped;

procedure Main is
   V1 : My_X := 5;
   V2 : My_X := 6;
begin
   Spec_Wrapped.My_P (V1);
   Other_Spec_Wrapped.My_P (V2);
end Main;
