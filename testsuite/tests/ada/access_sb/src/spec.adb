with Ada.Text_IO; use Ada.Text_IO;

package body Spec is


   procedure X (V : myCallback) is
   begin
      V (11, 222, 3333);
   end X;
   
end Spec;
