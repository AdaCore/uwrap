package Pck is

   type Rec is record
      X : access Integer;
   end record;

   procedure Print (V : Rec);

end Pck;
