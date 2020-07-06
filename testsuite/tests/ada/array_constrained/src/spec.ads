package Spec is
   
   type anEnum is (vA, vB, vC);
  
   type anArray is array (0 .. 2) of anEnum;
   
   procedure aProc (V : anArray);

end Spec;
