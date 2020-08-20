package Pck is

   type A_Type1 is (Lit1, Lit2, Lit3)
   with Convention => C;

   type A_Type2 is (Lit1, Lit2, Lit3)
   with Convention => C;
   for A_Type2 use (Lit1 => 99, Lit2 => 888, Lit3 => 7777);

   type R is record
      V : A_Type1;
      V2 : A_Type2;
      B1 : Boolean;
      B2 : Boolean;
      I1 : Integer;
      B3 : Boolean;
   end record
     with Convention => C_Pass_By_Copy;


   procedure P (V1 : A_Type1; V2 : A_Type2);

   procedure P2 (X : R);

end Pck;
