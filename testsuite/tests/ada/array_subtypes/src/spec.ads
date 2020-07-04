package Spec is

   type T is array (Integer range <>) of Integer;
   subtype T2 is T (1 .. 10);

   procedure P (V : T2);
   
end Spec;
