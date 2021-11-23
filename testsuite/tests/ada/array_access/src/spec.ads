package Spec is   
  
   type Some_Array is array (0 .. 2) of aliased access Integer;
   
   procedure Proc (V : Some_Array);

end Spec;
