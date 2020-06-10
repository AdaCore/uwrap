with Pck_Wrapped; use Pck_Wrapped;

procedure Main is
   X1 : The_Type := 9;
   X2 : The_Record := (1, 2, 3);
begin
   Pck_Wrapped.Proc (X1, X2);
end Main;
