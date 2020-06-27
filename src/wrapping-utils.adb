with Ada.Wide_Wide_Characters.Unicode; use Ada.Wide_Wide_Characters.Unicode;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with GNAT.Regpat; use GNAT.Regpat;

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

   function Unindent (Text : Text_Type) return Text_Type is
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
   end Unindent;

   function Suffix (Text : Text_Type) return Text_Type is
   begin
      for C in reverse Text'Range loop
         if Text (C) = '.' then
            if C = Text'Last then
               return "";
            else
               return Text (C + 1 .. Text'Last);
            end if;
         end if;
      end loop;

      return Text;
   end Suffix;

   function Replace_String
     (Source, Pattern, Replace : Text_Type) return Text_Type
   is
      Matcher : Pattern_Matcher := Compile (To_String (Pattern));
      Result  : Unbounded_Text_Type;
      Prev    : Integer := Source'First;
      Matches : Match_Array (0 .. Paren_Count (Matcher));

      Source_Str : String := To_String (Source);
   begin
      while Prev in Source'Range loop
         Match
           (Matcher,
            Source_Str (Prev .. Source'Last),
            Matches);

         if Matches (0) = No_Match then
            Append (Result, Source (Prev .. Source'Last));
            exit;
         else
            Append (Result, Source (Prev .. Matches (0).First - 1));
            Append (Result, Replace);

            Prev := Matches (0).Last + 1;
         end if;
      end loop;

      return To_Text (Result);
   end Replace_String;

end Wrapping.Utils;
