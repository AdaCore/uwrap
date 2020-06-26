package Spec is

   type T1 is range 1 .. 100;

   type T2 is new T1 range 9 .. 99;

   type T3 is access T1;

   type T4 is array (T1 range <>) of T2;

   type T5 is (A, B, C);

   type T6 is record
      V : T2;
   end record;

   procedure P (V : T1);

   subtype T7 is T1;

   subtype T8 is T2 range 10 .. 80;

end Spec;
