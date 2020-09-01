
   with test_h;

   use test_h;


   
      package body test_Wrapped is
        
   
   procedure s1 ( p1 :  String; p2 :  String )  is
      Temp_c_string_1 : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (p1);
Temp_c_string_3 : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (p2);

   begin
      
      declare
         
      begin
         test_h.s1 ( Temp_c_string_1, Temp_c_string_3 );
         declare
            
         begin
            Interfaces.C.Strings.Free (Temp_c_string_1);
Interfaces.C.Strings.Free (Temp_c_string_3);

            null;
         end;
      end;
   end s1;
   
   
   function s2    return String is
      
   begin
      
      declare
         Temp_c_string_1 : aliased Interfaces.C.Strings.chars_ptr := test_h.s2   ;

      begin
         null;
         declare
            
         begin
            
            return Interfaces.C.Strings.Value (Temp_c_string_1);
         end;
      end;
   end s2;
   
   
   function s3 ( arg1 :  String; arg2 :  String ) return String is
      Temp_c_string_2 : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (arg1);
Temp_c_string_4 : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (arg2);

   begin
      
      declare
         Temp_c_string_1 : aliased Interfaces.C.Strings.chars_ptr := test_h.s3 ( Temp_c_string_2, Temp_c_string_4 );

      begin
         null;
         declare
            
         begin
            Interfaces.C.Strings.Free (Temp_c_string_2);
Interfaces.C.Strings.Free (Temp_c_string_4);

            return Interfaces.C.Strings.Value (Temp_c_string_1);
         end;
      end;
   end s3;
   
   
   procedure s4 ( p1 :  String; leaveMeAlone :    Interfaces. C. Strings. chars_ptr )  is
      Temp_c_string_1 : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (p1);
Temp_local_4 : aliased Interfaces.C.Strings.chars_ptr with Address => leaveMeAlone'Address, Import;

   begin
      
      declare
         
      begin
         test_h.s4 ( Temp_c_string_1, Temp_local_4 );
         declare
            
         begin
            Interfaces.C.Strings.Free (Temp_c_string_1);

            null;
         end;
      end;
   end s4;
   
      begin
        null;
        
      end test_Wrapped;
   
   
