with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   G : aliased R := (5, 10);

   function F return access R is
   begin
      return G'Access;
   end F;

end Pck;
