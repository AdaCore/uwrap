with Pck_Wrapped; use Pck_Wrapped;

procedure Main is
   V : Some_Type;
begin
   V.B := 5;
   Pck_Wrapped.Some_Proc (V);
end Main;
