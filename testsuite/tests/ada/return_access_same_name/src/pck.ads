package Pck is

   type R is record
      A : Integer;
      B : Integer;
   end record;

   function F return access R;

end Pck;
