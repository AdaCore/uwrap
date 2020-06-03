with Ada.Wide_Wide_Characters.Unicode; use Ada.Wide_Wide_Characters.Unicode;

package body Wrapping.Utils is

   function Remove_Quotes (Text : Text_Type) return Text_Type is
   begin
      if Text'Length = 0 then
         return "";
      elsif Text (Text'First) /= '"' and then Text (Text'First) /= ''' then
         return Text;
      elsif Text'Length >= 6 and then Text (Text'First .. Text'First + 2) = """""""" then
         return Text (Text'First + 3 .. Text'Last - 3);
      else
         return Text (Text'First + 1 .. Text'Last - 1);
      end if;
   end Remove_Quotes;

   function Unident (Text : Text_Type) return Text_Type is
      Space_Count : Integer := 0;
      Max_Space_Count : Integer := 0;
      Result : Text_Type (Text'Range);
      Result_Index : Integer := Result'First - 1;
      Skip_To_Terminator : Boolean := False;
   begin
      for C of Text loop
         if C = ' ' then
            if not Skip_To_Terminator then
               Space_Count := Space_Count + 1;
            end if;
         elsif Is_Line_Terminator (C) then
            Space_Count := 0;
            Skip_To_Terminator := False;
         else
            if Space_Count > Max_Space_Count then
               Max_Space_Count := Space_Count;
            end if;

            Skip_To_Terminator := True;
         end if;
      end loop;

      for C of Text loop
         if C = ' ' then
            Space_Count := Space_Count + 1;
         elsif Is_Line_Terminator (C) then
            Space_Count := 0;
         end if;

         if C /= ' ' or else Space_Count > Max_Space_Count then
            Result_Index := Result_Index + 1;
            Result (Result_Index) := C;
         end if;
      end loop;

      return Result (Result'First .. Result_Index);
   end Unident;


end Wrapping.Utils;
