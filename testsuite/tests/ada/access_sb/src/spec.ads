
with Some_Types; use Some_Types;

package Spec is
   
   type myCallback is access procedure
        (arg1 : aType;
         arg2 : aType;
         arg3 : aType);
   
   procedure X (V : myCallback);

end Spec;
