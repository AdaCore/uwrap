with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   procedure Proc (V : An_Integer) is
   begin
      Put_Line (An_Integer'Image (V));
   end Proc;

end Pck;
