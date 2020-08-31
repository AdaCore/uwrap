package body Test is

   procedure P1 (V1 : access Integer) is
   begin
      P2 (V1);
      P3 (V1.all);
   end P1;

   procedure P2 (V2 : access Integer) is
   begin
      P3 (V2.all);
   end P2;

   procedure P3 (V3 : Integer) is
   begin
      null;
   end P3;

end Test;
