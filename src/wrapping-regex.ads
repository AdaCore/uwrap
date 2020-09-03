with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with GNAT.Regpat; use GNAT.Regpat;

package Wrapping.Regex is

   Unknown_Group_Name : exception;
   Bad_Group_Index    : exception;

   type Basic_Regex is private;

   package Match_Holder is new Ada.Containers.Indefinite_Holders (Match_Array);

   type Capture_Group is record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Index    : Natural;
      Location : GNAT.Regpat.Match_Location;
   end record;

   package Name_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Capture_Group);

   type Match_Obj is record
      Matches         : Match_Holder.Holder;
      Names           : Name_Vectors.Vector;
      Original_String : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Match_List_Container is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Match_Obj);

   function Compile (Pattern : String) return Basic_Regex;

   function Match (Self : Basic_Regex; Str : String) return Match_Obj;

   function No_Match (Self : Match_Obj) return Boolean;

   function Length (Self : Match_Obj) return Natural;

   function Get (Self : Match_Obj; Index : String) return String;

   function Get (Self : Match_Obj; Index : Natural) return String;

   function Get_Capture_Name (Self : Match_Obj; Index : Natural) return String;

private

   package Pattern_Holder is new Ada.Containers.Indefinite_Holders
     (Pattern_Matcher);

   type Basic_Regex is record
      Names   : Name_Vectors.Vector;
      Pattern : Pattern_Holder.Holder;
   end record;

   No_Group_Name : constant String := "";

   function Get_Noexcept (Self : Match_Obj; Index : String) return String;
end Wrapping.Regex;
