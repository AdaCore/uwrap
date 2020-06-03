with Ada.Text_IO; use Ada.Text_IO;

with Wrapping.Run;
with Wrapping; use Wrapping;

procedure UWrap is
begin
   Wrapping.Run.App.Run;
exception
   when Wrapping_Error =>
      Put_Line ("wrapping errors");
end UWrap;
