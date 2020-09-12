-- SPEC
package Spec is
-- X
X : Integer;
-- Y1
type Y1 is record
-- F1, F2
F1, F2 : Integer;
end record;
-- Y2
type Y2 is record
-- F3, F4
F3, F4 : Integer;
F5, F6 : Integer;
F7, F8 : Integer;
F9, F10 : Integer;
end record with Import, Convention => C;
-- P
procedure P (P1 : Integer; P2 : Y; P3 : Y; P4 : Y; P5 : Y; P6 : Y; P7 : Y; P8 : Y; P9 : Y);
end Spec;
-- AFTER SPEC