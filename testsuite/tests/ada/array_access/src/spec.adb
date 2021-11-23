with Ada.Text_IO; use Ada.Text_IO;

package body Spec is

   procedure Proc (V : Some_Array) is
   begin
      for X of V loop
         Put_Line (X.all'Img);
      end loop;
   end Proc;
   
end Spec;
