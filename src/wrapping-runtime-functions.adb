with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Wrapping.Utils; use Wrapping.Utils;

package body Wrapping.Runtime.Functions is

   procedure Build_Lambda
     (Object : access W_Object_Type'Class;
      Params : Argument_List)
   is
      A_Lambda : W_Lambda := W_Lambda (Object);
   begin
      A_Lambda.Params := Params;
      Capture_Lambda_Environment (A_Lambda, Params);
      Push_Object (A_Lambda);
   end Build_Lambda;

   procedure Normalize_Ada_Name
     (Object : access W_Object_Type'Class;
      Params : Argument_List)
   is
      New_Name : Unbounded_Text_Type;
      Prev_Up  : Boolean := False;
      Prev_Sep : Boolean := False;
   begin
      if Params.Children_Count /= 1 then
         Error ("expected one parameter");
      end if;

      declare
         Name : constant Text_Type :=
                  Evaluate_Expression
                    (Params.Child (1).As_Argument.F_Value).To_String;
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

         Push_Object (W_Object'(new W_String_Type'(Value => New_Name)));
      end;
   end Normalize_Ada_Name;

   overriding
   procedure Analyze_Parameters (Call : in out W_Call_To_Lower_Type; Params : Argument_List) is
   begin
      if Params.Children_Count /= 1 then
         Error ("to_lower needs to be called with one parameter");
      end if;

      Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

      Call.Param := Pop_Object;
   end Analyze_Parameters;

   -------------
   -- To_Text --
   -------------

   overriding function To_String
     (Object : W_Call_To_Lower_Type) return Text_Type
   is
   begin
      return To_Lower (Object.Param.To_String);
   end To_String;


   overriding
   procedure Analyze_Parameters (Call : in out W_Call_Unindent_Type; Params : Argument_List) is
   begin
      if Params.Children_Count /= 1 then
         Error ("unindent needs to be called with one parameter");
      end if;

      Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

      Call.Param := Pop_Object;
   end Analyze_Parameters;


   -------------
   -- To_Text --
   -------------

   overriding function To_String
     (Object : W_Call_Unindent_Type) return Text_Type
   is
   begin
      return Unident (Object.Param.To_String);
   end To_String;

end Wrapping.Runtime.Functions;
