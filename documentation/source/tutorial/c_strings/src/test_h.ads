pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package test_h is

   procedure s1 (p1 : Interfaces.C.Strings.chars_ptr; p2 : Interfaces.C.Strings.chars_ptr)  -- ../src/test.h:1
   with Import => True, 
        Convention => C, 
        External_Name => "s1";

   function s2 return Interfaces.C.Strings.chars_ptr  -- ../src/test.h:2
   with Import => True, 
        Convention => C, 
        External_Name => "s2";

   function s3 (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- ../src/test.h:3
   with Import => True, 
        Convention => C, 
        External_Name => "s3";

   procedure s4 (p1 : Interfaces.C.Strings.chars_ptr; leaveMeAlone : Interfaces.C.Strings.chars_ptr)  -- ../src/test.h:4
   with Import => True, 
        Convention => C, 
        External_Name => "s4";

end test_h;
