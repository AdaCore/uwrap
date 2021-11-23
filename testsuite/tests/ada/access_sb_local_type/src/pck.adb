with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   procedure Proc (Param : Proc_A) is
   begin
      Param.all (789);
   end Proc;

end Pck;
