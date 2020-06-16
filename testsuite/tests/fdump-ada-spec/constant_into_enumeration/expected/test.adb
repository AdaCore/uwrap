with test_h; use test_h;
package body Test is
   procedure S1 (P1 : String; P2 : String) is
      Tmp_1_C_String : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (P1);
      Tmp_2_C_String : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (P2);
   begin
      test_h.s1 (Tmp_1_C_String, Tmp_2_C_String);
      Interfaces.C.Strings.Free (Tmp_1_C_String);
      Interfaces.C.Strings.Free (Tmp_2_C_String);
   end S1;
   function S2 return String is
      Tmp_1_C_String : Interfaces.C.Strings.chars_ptr := test_h.s2;
   begin
      return Interfaces.C.Strings.Value (Tmp_1_C_String);
   end S2;
   function S3 (Arg1 : String; Arg2 : String) return String is
      Tmp_2_C_String : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (Arg1);
      Tmp_3_C_String : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (Arg2);
      Tmp_1_C_String : Interfaces.C.Strings.chars_ptr :=
        test_h.s3 (Tmp_2_C_String, Tmp_3_C_String);
   begin
      Interfaces.C.Strings.Free (Tmp_2_C_String);
      Interfaces.C.Strings.Free (Tmp_3_C_String);
      return Interfaces.C.Strings.Value (Tmp_1_C_String);
   end S3;
   procedure S4 (P1 : String; Leave_Me_Alone : Interfaces.C.Strings.chars_ptr)
   is
      Tmp_1_C_String : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (P1);
      Local_Tmp_0 : Interfaces.C.Strings.chars_ptr with
         Address => Leave_Me_Alone'Address,
         Import;
   begin
      test_h.s4 (Tmp_1_C_String, Local_Tmp_0);
      Interfaces.C.Strings.Free (Tmp_1_C_String);
   end S4;
begin
   null;
end Test;
