with Ada.Containers.Indefinite_Vectors;

with Langkit_Support.Text; use Langkit_Support.Text;

package Wrapping.Utils is

   function Remove_Quotes (Text : Text_Type) return Text_Type;
   -- Remove the quotes before and after the Text, taking into account both
   -- the signle line syntax and the multi lines one.

   package Text_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Text_Type);

   function Unident (Text : Text_Type) return Text_Type;
   --  Remove as many spaces in front of each lines as possible while keeping
   --  the current indentation

end Wrapping.Utils;
