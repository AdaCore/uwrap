package Pck is

   type A_Type is range 1 .. 100;

   type A_Record is record
      A : Integer;
      B : Integer;
      C : A_Type;
   end record;

   procedure Proc (P1 : A_Type; P2 : A_Record);

end Pck;
