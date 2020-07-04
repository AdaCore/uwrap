with Ada.Text_IO; use Ada.Text_IO;

package body Spec is


   procedure P (V : T2) is
   begin
      for I of V loop
         Put_Line (I'Img);
      end loop;
   end P;
   
end Spec;
