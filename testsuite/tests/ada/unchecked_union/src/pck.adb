with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   procedure someProc (V : someType) is
   begin
      Put_Line (V.A'Img);
   end someProc;

end Pck;
