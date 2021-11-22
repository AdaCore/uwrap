with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   procedure Print (V : Rec) is
   begin
      Put_Line (V.X.all'Img);
   end Print;

end Pck;
