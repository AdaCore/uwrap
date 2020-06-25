with Ada.Text_IO; use Ada.Text_IO;

package body Spec is

   procedure AProcedure (a_parameter : Integer; anotherParameter : Integer) is
   begin
      Put_Line (a_parameter'Img & ", " & anotherParameter'Img);
   end AProcedure;

end Spec;
