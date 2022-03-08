with Pck_Wrapped; use Pck_Wrapped;

procedure Main is
   function C ( X : out Some_Int ) return Some_Int is
   begin
      X := 98;
      return 998;
   end C;

   function S is new Pck_Wrapped.Something_Gen (C);

   V : Pck_Wrapped.Struct;
begin
   V.Sth := S'Unrestricted_Access;

   Pck_Wrapped.Call (V);
end Main;
