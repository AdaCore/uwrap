
   with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;

   

   
      package test_Wrapped is
        
        
   
   procedure s1 ( p1 :  String; p2 :  String ) ;
   
   
   function s2    return String;
   
   
   function s3 ( arg1 :  String; arg2 :  String ) return String;
   
   
   procedure s4 ( p1 :  String; leaveMeAlone :    Interfaces. C. Strings. chars_ptr ) ;
   
      end test_Wrapped; 
      
   
