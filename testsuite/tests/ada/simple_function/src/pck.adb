with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   function A_Function (A_Parameter_1 : A_Type; A_Parameter_2 : A_Type) return A_Type is
   begin
      return A_Parameter_1 + A_Parameter_2;
   end A_Function;

end Pck;
