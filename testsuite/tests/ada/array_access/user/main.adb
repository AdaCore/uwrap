with Spec_Wrapped; use Spec_Wrapped;

procedure Main is
   A, B, C : aliased Integer;
begin
   A := 1;
   B := 20;
   C := 300;

   Spec_Wrapped.Proc ((A'Unchecked_Access, B'Unchecked_Access, C'Unchecked_Access));
end Main;
