package Pck is

   type An_Integer is new Integer;

   type Proc_A is access procedure (V : An_Integer);

   procedure Proc (Param : Proc_A);

end Pck;
