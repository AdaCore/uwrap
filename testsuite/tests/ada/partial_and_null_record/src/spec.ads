package Spec is

   type T1;

   type A is access all T1;

   type T2 is record
      null;
   end record;

   procedure V (X : T1; Y : T2);

   type T1 is null record;

end Spec;
