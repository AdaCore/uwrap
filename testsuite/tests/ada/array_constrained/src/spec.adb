with Ada.Text_IO; use Ada.Text_IO;

package body Spec is

   procedure aProc (V : anArray) is
   begin
      for X of V loop
         Put_Line (X'Img);
      end loop;
   end aProc;
   
end Spec;
