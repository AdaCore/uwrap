with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with Langkit_Support.Text; use Langkit_Support.Text;

package Wrapping.Utils is

   package Text_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Text_Type);
   package Text_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Text_Type);

   package Integer_Vector is new Ada.Containers.Vectors (Positive, Integer);

   function Remove_Quotes (Text : Text_Type) return Text_Type;
   -- Remove the quotes before and after the Text, taking into account both
   -- the signle line syntax and the multi lines one.

   package Text_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Text_Type);

   function Unindent (Text : Text_Type) return Text_Type;
   --  Remove as many spaces in front of each lines as possible while keeping
   --  the current indentation

   function Suffix (Text : Text_Type) return Text_Type;

   function Replace_String
     (Source, Pattern, Replace : Text_Type) return Text_Type;

end Wrapping.Utils;
