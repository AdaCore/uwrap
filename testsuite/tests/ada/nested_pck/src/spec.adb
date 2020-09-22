with Ada.Text_IO; use Ada.Text_IO;

package body Spec is

   procedure A is
   begin
      Put_Line ("A");
   end A;
   
   package body Nested is
      
      procedure B is
      begin
         Put_Line ("B");
      end B;
      
   end Nested;

end Spec;
