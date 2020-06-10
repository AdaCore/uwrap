with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C;
package Test is
   procedure S1 (P1 : String; P2 : String);
   function S2 return String;
   function S3 (Arg1 : String; Arg2 : String) return String;
   procedure S4 (P1 : String; Leave_Me_Alone : Interfaces.C.Strings.chars_ptr);
end Test;
