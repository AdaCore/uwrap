with Spec_Wrapped; use Spec_Wrapped;

procedure Main is
   X : Spec_Wrapped.A_Type_Name := (1, 2);
begin
   Spec_Wrapped.A_Variable_Name := 3;

   Spec_Wrapped.A_Procedure (X.Field1, X.Field2);
   Spec_Wrapped.A_Procedure
     (Spec_Wrapped.A_Variable_Name,
      Spec_Wrapped.A_Variable_Name);
end Main;
