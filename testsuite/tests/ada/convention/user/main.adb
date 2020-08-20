with Ada.Text_IO; use Ada.Text_IO;

with Pck_Wrapped; use Pck_Wrapped;

procedure Main is
begin
   P_Wrapped (Literal_1, Literal_2);
   P2_Wrapped
     (X_Wrapped =>
        (V_Wrapped  => Literal_1,
         V2_Wrapped => Literal_2,
         B1_Wrapped => True,
         B2_Wrapped => False,
         I1_Wrapped => 99,
         B3_Wrapped => True));
end Main;
