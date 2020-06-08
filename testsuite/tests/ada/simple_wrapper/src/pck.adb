with Ada.Text_IO; use Ada.Text_IO;

package body Pck is

   procedure Proc1Suffix is
   begin
      Put_Line ("PROC 1 SUFFIX");
   end Proc1Suffix;

   procedure Proc2Suffix is
   begin
      Put_Line ("PROC 2 SUFFIX");
   end Proc2Suffix;

   procedure SomethingElse is
   begin
      Put_Line ("SOMETHING ELSE");
   end SomethingElse;

end Pck;
