with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   procedure Call (V : Struct) is
      Val : aliased Some_Int;
      Val2 : Some_Int;
   begin
      Val2 := V.Sth (Val'Unchecked_Access);
      Put_Line (Val'Img);
      Put_Line (Val2'Img);
   end Call;

end Pck;
