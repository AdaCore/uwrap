with Ada.Text_IO; use Ada.Text_IO;
with Pck_Wrapped; use Pck_Wrapped;

procedure Main is
   procedure My_Proc (V : access W_Integer) is
   begin
      Put_Line (W_Integer'Image (V.all + 1));
   end My_Proc;

   procedure My_Proc_Low is new Pck_Wrapped.W_Func_Gen (My_Proc);
begin
   Pck_Wrapped.Print (W_Func'(My_Proc_Low'Unrestricted_Access));
end Main;
