with Ada.Text_IO; use Ada.Text_IO;
with Spec_Wrapped; use Spec_Wrapped;
with My_Types; use My_Types;

procedure Main is

   procedure P ( Arg1 :   A_Type; Arg2 :   A_Type; Arg3 :   A_Type ) is
   begin
      Put_Line (Arg1'Img & ", "  & Arg2'Img & ", " & Arg3'Img);
   end P;

   procedure P_Gen is new My_Callback_Gen (P);

begin
   Spec_Wrapped.X (P_Gen'Unrestricted_Access);
end Main;
