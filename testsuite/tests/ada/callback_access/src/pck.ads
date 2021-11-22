with Interfaces.C; use Interfaces.C;

package Pck is

   type Func is access procedure (V1 : access Interfaces.C.int);   

   procedure Print (V : Func);

end Pck;
