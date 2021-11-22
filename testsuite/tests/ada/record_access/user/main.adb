with Pck_Wrapped; use Pck_Wrapped;

procedure Main is
   V : W_Rec;
begin
   V.X := new Integer;
   V.X.all := 99;
   Print (V);
end Main;
