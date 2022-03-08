package Pck is

   type Some_Int is new Integer;

   type Something is access function (X : access Some_Int) return Some_Int;

   type Struct is record
      Sth : Something;
   end record;

   procedure Call (V : Struct);

end Pck;
