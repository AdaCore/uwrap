package P is

   type Rec is record
      A : Integer;
      B : Integer;
   end record;

   function Some_F return Rec;

   procedure Some_P (V : Rec);

end P;
