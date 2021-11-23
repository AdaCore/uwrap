with Ada.Text_IO; use Ada.Text_IO;
with Pck_Wrapped; use Pck_Wrapped;
with Pck;

procedure Main is
  procedure Callback ( V : Pck_Wrapped.An_Integer ) is
  begin
     Put_Line (V'Img);
  end Callback;

  procedure Callback_Converted is new Pck_Wrapped.W_Proc_A_Gen (Callback);

begin
   Proc (W_Proc_A'(Callback_Converted'Unrestricted_Access));
end Main;
