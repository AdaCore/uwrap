package Pck is

   type A_Type1 is (Lit1, Lit2, Lit3);

   type A_Type2 is (Lit1, Lit2, Lit3);
   for A_Type2 use (Lit1 => 99, Lit2 => 888, Lit3 => 7777);

   procedure P (V1 : A_Type1; V2 : A_Type2);

end Pck;
