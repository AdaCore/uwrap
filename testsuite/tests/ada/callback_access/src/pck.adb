package body Pck is

   procedure Print (V : Func) is
      X : aliased Interfaces.C.int := 999;
   begin
      V (X'Access);
   end Print;

end Pck;
