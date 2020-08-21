with Ada.Text_IO; use Ada.Text_IO;

package body P is

   function Some_F return Rec is
   begin
      return (1, 2);
   end Some_F;

   procedure Some_P (V : Rec) is
   begin
      null;
   end Some_P;

end P;
