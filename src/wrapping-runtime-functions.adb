with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Wrapping.Utils; use Wrapping.Utils;

package body Wrapping.Runtime.Functions is

   procedure Call_Build_Lambda
     (Object : access W_Object_Type'Class;
      Params : Argument_List)
   is
      A_Lambda : W_Lambda := W_Lambda (Object);
   begin
      A_Lambda.Params := Params;
      Capture_Lambda_Environment (A_Lambda, Params);
      Push_Object (A_Lambda);
   end Call_Build_Lambda;

   P_Normalize_Ada_Name : Parameter_Profile :=
      (Make_Parameter ("str", False),
       Make_Parameter ("match", True));

   procedure Call_Normalize_Ada_Name
     (Object : access W_Object_Type'Class;
      Params : Argument_List)
   is
      New_Name : Unbounded_Text_Type;
      Prev_Up  : Boolean := False;
      Prev_Sep : Boolean := False;

      Actuals : Actuals_Type :=
        Process_Parameters (P_Normalize_Ada_Name, Params);
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode := Match_None;

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


         Push_Match_Result
           (W_Object'(new W_String_Type'(Value => New_Name)),
            Actuals (2));
      end;

      Pop_Frame_Context;
   end Call_Normalize_Ada_Name;

   P_Replace_Text : Parameter_Profile :=
      (Make_Parameter ("source", False),
       Make_Parameter ("pattern", False),
       Make_Parameter ("replace", False),
       Make_Parameter ("match", True));

   procedure Call_Replace_Text
     (Object : access W_Object_Type'Class;
      Params : Argument_List)
   is
      Result : W_Object;
      Actuals : Actuals_Type :=
        Process_Parameters (P_Replace_Text, Params);
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode := Match_None;

      declare
         Source  : Text_Type := Evaluate_Expression (Actuals (1)).To_String;
         Pattern : Text_Type := Evaluate_Expression (Actuals (2)).To_String;
         Replace : Text_Type := Evaluate_Expression (Actuals (3)).To_String;
      begin
         Result := (new W_String_Type'
                      (Value => To_Unbounded_Text
                       (Replace_String (Source, Pattern, Replace))));
      end;

      Push_Match_Result (Result, Actuals (4));
      Pop_Frame_Context;
   end Call_Replace_Text;

   P_To_Lower : Parameter_Profile :=
      (Make_Parameter ("str", False),
       Make_Parameter ("match", True));

   procedure Call_To_Lower
    (Object : access W_Object_Type'Class; Params : Argument_List)
   is
      Result : W_Object;

      Actuals          : Actuals_Type :=
        Process_Parameters (P_To_Lower, Params);
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode := Match_None;

      Result :=
        new W_String_Type'
          (Value => To_Unbounded_Text
             (To_Lower (Evaluate_Expression (Actuals (1)).To_String)));

      Push_Match_Result (Result, Actuals (2));

      Pop_Frame_Context;
   end Call_To_Lower;


   P_Unindent : Parameter_Profile :=
      (Make_Parameter ("str", False),
       Make_Parameter ("match", True));

   procedure Call_Unindent
    (Object : access W_Object_Type'Class; Params : Argument_List)
   is
      Result : W_Object;

      Actuals          : Actuals_Type :=
        Process_Parameters (P_Unindent, Params);
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode := Match_None;

      Result :=
        new W_String_Type'
          (Value => To_Unbounded_Text
             (Unindent (Evaluate_Expression (Actuals (1)).To_String)));

      Push_Match_Result (Result, Actuals (2));

      Pop_Frame_Context;
   end Call_Unindent;

end Wrapping.Runtime.Functions;
