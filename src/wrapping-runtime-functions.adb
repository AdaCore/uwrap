with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Wrapping.Utils; use Wrapping.Utils;

package body Wrapping.Runtime.Functions is

   P_Normalize_Ada_Name : Parameter_Profile :=
      (1 => Make_Parameter ("str", False));

   procedure Call_Normalize_Ada_Name
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
      New_Name : Unbounded_Text_Type;
      Prev_Up  : Boolean := False;
      Prev_Sep : Boolean := False;

      Actuals : Actuals_Type :=
        Process_Parameters (P_Normalize_Ada_Name, Params);
   begin
      declare
         Name : constant Text_Type :=
            Evaluate_Expression (Actuals (1)).To_String;
         C    : Integer := Name'First;
      begin
         while C <= Name'Last loop
            if Is_Upper (Name (C)) then
               if not Prev_Up and then not Prev_Sep and then
                 Length (New_Name) > 0
               then
                  Append (New_Name, "_");
                  Prev_Sep := True;
               end if;

               Prev_Up := True;
            else
               Prev_Up := False;
            end if;

            if Length (New_Name) = 0 or else Prev_Sep then
               Append (New_Name, To_Upper (Name (C)));
            else
               Append (New_Name, Name (C));
            end if;

            if Name (C) = '_' or else Name (C) = '.' then
               Prev_Sep := True;
            else
               Prev_Sep := False;
            end if;

            C := C + 1;
         end loop;
      end;

      Push_Object (To_W_String (New_Name));
   end Call_Normalize_Ada_Name;

   P_Replace_Text : Parameter_Profile :=
      (Make_Parameter ("source", False),
       Make_Parameter ("pattern", False),
       Make_Parameter ("replace", False));

   procedure Call_Replace_Text
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
      Result : W_Object;
      Actuals : Actuals_Type :=
        Process_Parameters (P_Replace_Text, Params);
   begin
      declare
         Source  : Text_Type := Evaluate_Expression (Actuals (1)).To_String;
         Pattern : Text_Type := Evaluate_Expression (Actuals (2)).To_String;
         Replace : Text_Type := Evaluate_Expression (Actuals (3)).To_String;
      begin
         Result := W_Object (To_W_String (Replace_String (Source, Pattern, Replace)));
      end;

      Push_Object (Result);
   end Call_Replace_Text;

   P_To_Lower : Parameter_Profile :=
      (1 => Make_Parameter ("str", False));

   procedure Call_To_Lower
    (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Result : W_Object;

      Actuals          : Actuals_Type :=
        Process_Parameters (P_To_Lower, Params);
   begin
      Result := W_Object (To_W_String (To_Lower (Evaluate_Expression (Actuals (1)).To_String)));
      Push_Object (Result);
   end Call_To_Lower;

   P_Unindent : Parameter_Profile :=
     (Make_Parameter ("ident", False),
      Make_Parameter ("str", False));

   procedure Call_Reindent
    (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Result : W_Object;

      Actuals          : Actuals_Type :=
        Process_Parameters (P_Unindent, Params);
      Indentation : W_Object;
   begin
      Indentation := Evaluate_Expression (Actuals (1)).Dereference;

      if Indentation.all not in W_Integer_Type'Class then
         Error ("expected integer object");
      end if;

      Result :=
        W_Object
          (To_W_String
             (Reindent
                (W_Integer (Indentation).Value,
                 Evaluate_Expression (Actuals (2)).To_String,
                 True)));
      Push_Object (Result);
   end Call_Reindent;

end Wrapping.Runtime.Functions;
