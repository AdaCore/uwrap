package Pck is

   type myInt is new Integer;

   type someType (discr : myInt := 0) is record
      case discr is
         when 0 =>
            A : aliased myInt;
         when 1 =>
            B : aliased myInt;
         when 2 =>
            C : aliased myInt;
         when others =>
            D : aliased myInt;
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;

   procedure someProc (V : someType);

end Pck;
