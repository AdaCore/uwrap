with Ada.Containers; use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;
with Wrapping.Semantic.Structure;
with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Wrapping.Runtime.Functions; use Wrapping.Runtime.Functions;

package body Wrapping.Runtime.Structure is

   Root_Language_Entities : Language_Entity_Maps.Map;

   function Get_Visible_Symbol (A_Frame: Data_Frame_Type; Name : Text_Type) return Runtime_Object is
   begin
      if A_Frame.Symbols.Contains (Name) then
         return A_Frame.Symbols.Element (Name);
      elsif A_Frame.Parent_Frame /= null then
         return Get_Visible_Symbol (A_Frame.Parent_Frame.all, Name);
      else
         --  Return one of the global function.
         --  TODO: we probably want some mechanism to allow this to be extended
         if Name = "unindent" then
            return new Runtime_Call_Unindent_Type;
         elsif Name = "to_lower" then
            return new Runtime_Call_To_Lower_Type;
         else
            return null;
         end if;
      end if;
   end Get_Visible_Symbol;

   function Get_Module (A_Frame : Data_Frame_Type) return Semantic.Structure.Module is
      use Semantic.Structure;

      Scope : Semantic.Structure.Entity := A_Frame.Lexical_Scope;
   begin
      while Scope /= null and then Scope.all not in Module_Type'Class loop
         Scope := Scope.Parent;
      end loop;

      return Semantic.Structure.Module (Scope);
   end Get_Module;

   procedure Add_Child (Parent, Child : access Language_Entity_Type'Class) is
   begin
      Child.Parent := Language_Entity (Parent);

      if Parent.Children_Ordered.Length > 0 then
         Parent.Children_Ordered.Last_Element.Next := Language_Entity (Child);
         Child.Prev := Parent.Children_Ordered.Last_Element;
      end if;

      Parent.Children_Ordered.Append (Language_Entity (Child));
   end Add_Child;

   procedure Add_Child (Parent, Child : access Language_Entity_Type'Class; Name : Text_Type) is
   begin
      Add_Child (Parent, Child);
      Parent.Children_Indexed.Insert (Name, Language_Entity (Child));
   end Add_Child;

   function Get_Root_Language_Entity (Language : Text_Type) return Language_Entity is
      Root_Entity : Language_Entity;
   begin
      if not Root_Language_Entities.Contains (Language) then
         Root_Entity := new Language_Entity_Type;
         Root_Language_Entities.Insert (Language, Root_Entity);
      else
         Root_Entity := Root_Language_Entities.Element (Language);
      end if;

      return Root_Entity;
   end Get_Root_Language_Entity;

   procedure Stack_Parameters
     (A_Profile         : Profile_Type;
      Actual_Parameters : Argument_List)
   is
      Parameter_Index : Integer := 1;
      In_Named_Section : Boolean := False;

      Name : Unbounded_Text_Type;
      Expressions : array (1 .. Integer (A_Profile.Parameters_By_Position.Length)) of Template_Node;
      A_Runtime_Expression : Runtime_Expression;
   begin
      for Param of Actual_Parameters loop
         if Parameter_Index > Integer (A_Profile.Parameters_By_Position.Length) then
            Error ("too many parameters");
         elsif not Param.As_Argument.F_Name.Is_Null then
            In_Named_Section := True;

            Parameter_Index := A_Profile.Parameters_By_Name.Element
              (Param.As_Argument.F_Name.Text).Index;
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;
         end if;

         Expressions (Parameter_Index) := Param.F_Value;

         if not In_Named_Section then
            Parameter_Index := Parameter_Index + 1;
         end if;
      end loop;

      for Parameter_Index in Expressions'Range loop
         if A_Profile.Parameters_By_Position.Element (Parameter_Index).A_Data_Type = Expression then
            A_Runtime_Expression := new Runtime_Expression_Type;
            A_Runtime_Expression.Expression := Expressions (Parameter_Index);
            Top_Frame.Data_Stack.Append (Runtime_Object (A_Runtime_Expression));
         else
            Evaluate_Expression (Expressions (Parameter_Index));
         end if;
      end loop;
   end Stack_Parameters;

   function Push_Value
     (An_Entity : access Language_Entity_Type;
      Name      : Text_Type) return Boolean
   is
   begin
      if An_Entity.Templates_By_Name.Contains (Name) then
         Push_Entity (An_Entity.Templates_By_Name.Element (Name));

         return True;
      elsif Name = "parent" or else Name = "child" or else Name = "next" or else Name = "prev" then
         Top_Frame.Data_Stack.Append
           (new Runtime_Function_Reference_Type'
              (Name   => To_Unbounded_Text (Name),
               Prefix => Language_Entity (An_Entity)));
      end if;

      return False;
   end Push_Value;

   function Push_Browse_Result
     (An_Entity : access Language_Entity_Type'Class;
      Name      : Text_Type;
      Params    : Argument_List) return Boolean
   is
      A_Mode : Browse_Mode;
   begin
      if Name = "parent" then
         A_Mode := Parent;

         if Params.Children_Count = 0 then
            if An_Entity.Parent /= null then
               Push_Match_True (An_Entity);
            else
               Push_Match_False;
            end if;
         end if;
      elsif Name = "child" then
         A_Mode := Child_Breadth;

         if Params.Children_Count = 0 then
            if An_Entity.Children_Indexed.Length > 0 then
               Push_Match_True (An_Entity);
            else
               Push_Match_False;
            end if;
         end if;
      elsif Name = "prev" then
         A_Mode := Prev;

         if Params.Children_Count = 0 then
            if An_Entity.Prev /= null then
               Push_Match_True (An_Entity);
            else
               Push_Match_False;
            end if;
         end if;
      elsif Name = "next" then
         A_Mode := Next;

         if Params.Children_Count = 0 then
            if An_Entity.Next /= null then
               Push_Match_True (An_Entity);
            else
               Push_Match_False;
            end if;
         end if;
      elsif Name = "sibling" then
         A_Mode := Sibling;

         if Params.Children_Count = 0 then
            if An_Entity.Prev /= null or else An_Entity.Next /= null then
               Push_Match_True (An_Entity);
            else
               Push_Match_False;
            end if;
         end if;
      elsif Name = "template" then
          A_Mode := Template;

         if Params.Children_Count = 0 then
            if An_Entity.Templates_By_Name.Length > 0 then
               Push_Match_True (An_Entity);
            else
               Push_Match_False;
            end if;
         end if;
      else
         return False;
      end if;

      if Params.Children_Count = 1 then
         Evaluate_Bowse_Functions (An_Entity, A_Mode, Params.Child (1).As_Argument.F_Value);
      elsif Params.Children_Count > 1 then
         Error ("matcher takes only 1 argument");
      end if;

      return True;
   end Push_Browse_Result;

   function Push_Call_Result
     (An_Entity : access Language_Entity_Type;
      Name      : Text_Type;
      Params    : Argument_List) return Boolean
   is
   begin
     return Push_Browse_Result (An_Entity, Name, Params);
   end Push_Call_Result;

   function Push_Match_Result
     (An_Entity : access Language_Entity_Type;
      Selector  : Runtime_Object;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean
   is
   begin
      return Push_Browse_Result (An_Entity, Selector.To_Text, Params);
   end Push_Match_Result;

   function Traverse
     (An_Entity : access Language_Entity_Type;
      A_Mode    : Browse_Mode;
      Include_Self : Boolean;
      Visitor   : access function (E : access Language_Entity_Type'Class) return Visit_Action)
      return Visit_Action
   is
      Current : Language_Entity;
      Current_Children_List : Language_Entity_Vectors.Vector;
      Next_Children_List : Language_Entity_Vectors.Vector;
   begin
      Language_Entity_Type'Class (An_Entity.all).Pre_Visit;

      if Include_Self then
         Language_Entity_Type'Class (An_Entity.all).Pre_Visit;

         case Visitor (An_Entity) is
            when Stop =>
               return Stop;

            when Over =>
               return Over;

            when Into =>
               null;

            when Unknown =>
               null;
         end case;
      end if;

      if A_Mode = Sibling then
         case Language_Entity_Type'Class (An_Entity.all).Traverse
           (Prev,
            False,
            Visitor)
         is
            when Stop =>
               return Stop;

            when Over =>
               return Over;

            when Into =>
               null;

            when Unknown =>
               null;
         end case;

         return Language_Entity_Type'Class (An_Entity.all).Traverse
           (Next,
            False,
            Visitor);
      elsif A_Mode = Template then
         for T of An_Entity.Templates_Ordered loop
            Language_Entity_Type'Class (T.all).Pre_Visit;

            case Visitor (T) is
               when Over =>
                  null;

               when Stop =>
                  return Stop;

               when Into =>
                  null;

               when Unknown =>
                  null;
            end case;
         end loop;

         return Into;
      end if;

      case A_Mode is
         when Parent =>
            Current := An_Entity.Parent;

         when Next =>
            Current := An_Entity.Next;

         when Prev =>
            Current := An_Entity.Prev;

         when Child_Depth =>
            if An_Entity.Children_Ordered.Length > 0 then
               Current := An_Entity.Children_Ordered.First_Element;
            end if;

         when Child_Breadth =>
            for C of An_Entity.Children_Ordered loop
               Current_Children_List.Append (C);
            end loop;

         when Sibling | Template =>
            null;

      end case;

      if A_Mode = Child_Breadth then
         loop
            for C of Current_Children_List loop
               Language_Entity_Type'Class (C.all).Pre_Visit;

               case Visitor (C) is
                  when Stop =>
                     return Stop;

                  when Over =>
                     null;

                  when Into =>
                     for C2 of C.Children_Ordered loop
                        Next_Children_List.Append (C2);
                     end loop;

                  when Unknown =>
                     null;
               end case;
            end loop;

            exit when Next_Children_List.Length = 0;

            Current_Children_List.Clear;
            Current_Children_List.Assign (Next_Children_List);
            Next_Children_List.Clear;
         end loop;
      else
         while Current /= null loop
            Language_Entity_Type'Class (Current.all).Pre_Visit;

            case Visitor (Current) is
               when Stop =>
                  return Stop;

               when Over =>
                  null;

               when Into =>
                  if A_Mode = Child_Depth then
                     case Language_Entity_Type'Class (Current.all).Traverse (A_Mode, False, Visitor) is
                        when Stop =>
                           return Stop;

                        when others =>
                           null;
                     end case;
                  end if;

               when Unknown =>
                  null;
            end case;

            case A_Mode is
               when Parent =>
                  Current := Current.Parent;

               when Prev =>
                  Current := Current.Prev;

               when Next | Child_Depth =>
                  Current := Current.Next;

               when Child_Breadth =>
                  null;

               when Sibling | Template =>
                  null;

            end case;
         end loop;
      end if;

      return Into;
   end Traverse;

   procedure Evaluate_Bowse_Functions
     (An_Entity                 : access Language_Entity_Type;
      A_Mode                    : Browse_Mode;
      Match_Expression          : Template_Node;
      Evaluate_Match_Expression : access procedure := null)
   is
      function Visitor (E : access Language_Entity_Type'Class) return Visit_Action is
         Result : Runtime_Object;
      begin
         --  There is a subtetly in the browsing functions. The self reference
         --  within these calls isn't the entity currently analyzed anymore but
         --  directly the entity that is being evaluated under these calls.
         --  However, we cannot create a sub frame as whatever we match needs
         --  to find its way to the command frame (otherwise any extracted
         --  group would be deleted upon frame popped).
         --  TODO: these specificities needs to be duly documented in the UG.
         Push_Implicit_Self (E);

         if Evaluate_Match_Expression = null then
            Evaluate_Expression (Match_Expression);
         else
            Evaluate_Match_Expression.all;
         end if;

         Result := Top_Frame.Data_Stack.Last_Element;
         Top_Frame.Data_Stack.Delete_Last (2);

         if Result /= Match_False then
            Push_Match_True (E);
            return Stop;
         else
            return Into;
         end if;
      end Visitor;

   begin
      if Language_Entity_Type'Class(An_Entity.all).Traverse (A_Mode, False, Visitor'Access) /= Stop then
         --  If we browsed the tree without finding a match, then push false.
         Push_Match_False;
      else
         -- Otherwise, delete the match and push the current entity
         Top_Frame.Data_Stack.Delete_Last;
         Push_Match_True (An_Entity);
      end if;
   end Evaluate_Bowse_Functions;


   procedure Push_Match_True (An_Entity : access Language_Entity_Type) is
   begin
      Push_Entity (An_Entity);
   end Push_Match_True;

   procedure Push_Match_True (An_Entity : access Runtime_Object_Type'Class) is
   begin
      if An_Entity.all not in Runtime_Language_Entity_Type'Class then
         Error ("expected language entity type");
      else
         Push_Match_True (Runtime_Language_Entity_Type (An_Entity.all).Value);
      end if;
   end Push_Match_True;

   procedure Push_Match_False is
   begin
      Top_Frame.Data_Stack.Append (Match_False);
   end Push_Match_False;

   procedure Print (An_Entity : Language_Entity; Indent : Text_Type := "") is
   begin
      Put_Line (Indent & An_Entity.To_Text);

      for E of An_Entity.Children_Ordered loop
         Print (E, Indent & "-");
      end loop;
   end Print;

   function Create_Template_Instance
     (An_Entity : access Language_Entity_Type'Class;
      A_Template : Semantic.Structure.Template) return Template_Instance
   is
      New_Template : Template_Instance;
   begin
      New_Template := new Template_Instance_Type;
      New_Template.Template := A_Template;
      New_Template.Origin := Language_Entity (An_Entity);

      An_Entity.A_Class := Language_Class_Registry.Element ("template");
      An_Entity.Templates_By_Name.Insert (A_Template.Name_Node.Text, New_Template);
      An_Entity.Templates_By_Full_Id.Insert (A_Template.Full_Name, New_Template);
      An_Entity.Templates_Ordered.Append (New_Template);

      Add_Child (Get_Root_Language_Entity ("template_tmp"), New_Template);

      return New_Template;
   end Create_Template_Instance;

   function Get_Template_Instance
     (An_Entity : access Language_Entity_Type'Class;
      Name      : Text_Type) return Template_Instance
   is
   begin
      if An_Entity.Templates_By_Name.Contains (Name) then
         return An_Entity.Templates_By_Name.Element (Name);
      else
         return null;
      end if;
   end Get_Template_Instance;

   function Get_Template_Instance
     (An_Entity  : access Language_Entity_Type'Class;
      A_Template : Semantic.Structure.Template) return Template_Instance is
   begin
      --  TODO: These calls to full name may be very costly, it'd be better
      --  to cache the full name in the object
      if An_Entity.Templates_By_Full_Id.Contains (A_Template.Full_Name) then
         return An_Entity.Templates_By_Full_Id.Element (A_Template.Full_Name);
      else
         return null;
      end if;
   end Get_Template_Instance;

   function Push_Value
     (An_Entity : access Template_Instance_Type;
      Name      : Text_Type) return Boolean
   is
      use Wrapping.Semantic.Structure;

      Named_Entity : Entity;
      Result : Runtime_Object;
   begin
      if Language_Entity_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      -- First cover the case of a variable or a pattern

      if Name = "origin" then
         Push_Entity (An_Entity.Origin);

         return True;
      else
         Named_Entity := An_Entity.Template.Get_Component (Name);

         if Named_Entity /= null then
            if Named_Entity.all in Var_Type then
               declare
                  A_Var : Semantic.Structure.Var :=  Semantic.Structure.Var (Named_Entity);
               begin
                  if A_Var.Kind = Text_Kind then
                     if Top_Frame.Context /= Match_Context then
                        if not An_Entity.Symbols.Contains (Name) then
                           An_Entity.Symbols.Insert (Name, new Runtime_Text_Container_Type);
                        end if;

                        Top_Frame.Data_Stack.Append (An_Entity.Symbols.Element (Name));

                        return True;
                     else
                        Top_Frame.Data_Stack.Append
                          (new Runtime_Function_Reference_Type'
                             (Name   => To_Unbounded_Text (Name),
                              Prefix => Language_Entity (An_Entity)));

                        return True;
                     end if;
                  elsif A_Var.Kind = Pattern_Kind then
                     --  If it's a pattern, just return the current value of the result
                     --  as a text expression. This will need to be evaluated, push
                     --  self on the stack

                     Push_Entity (An_Entity);

                     Evaluate_Expression (A_Var.Args.Child (1).As_Argument.F_Value);

                     Result := Top_Frame.Data_Stack.Last_Element;
                     Top_Frame.Data_Stack.Delete_Last (2);

                     Top_Frame.Data_Stack.Append (Result);

                     return True;
                  end if;
               end;
            end if;
         end if;
      end if;

      return False;
   end Push_Value;

   function Push_Call_Result
     (An_Entity : access Template_Instance_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean
   is

   begin
      if Language_Entity_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      return False;
   end Push_Call_Result;

   function Push_Match_Result
     (An_Entity : access Template_Instance_Type;
      Selector  : Runtime_Object;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean
   is
      use Wrapping.Semantic.Structure;

      procedure Match_Variable_Value (Name : Text_Type; Expression : Libtemplatelang.Analysis.Template_Node) is
         Result : Runtime_Object;
         Matched : Boolean;
      begin
         Evaluate_Expression (Expression);

         Result := Top_Frame.Data_Stack.Last_Element;
         Top_Frame.Data_Stack.Delete_Last;

         if An_Entity.Symbols.Contains (Name) then
            Matched := Runtime.Analysis.Match (Result.To_Text, An_Entity.Symbols.Element (Name).To_Text);
         else
            Matched := Runtime.Analysis.Match (Result.To_Text, "");
         end if;

         if Matched then
            Push_Match_True (An_Entity);
         else
            Push_Match_False;
         end if;
      end Match_Variable_Value;

      A_Named_Entity : Named_Entity;
      Result : Runtime_Object;

      function Is_Instance_Of (A_Template : Template_Type'Class; Name : Text_Type) return Boolean is
      begin
         if A_Template.Name_Node.Text = Name then
            return True;
         elsif A_Template.Extends /= null then
            return Is_Instance_Of (A_Template.Extends.all, Name);
         else
            return False;
         end if;
      end Is_Instance_Of;

      Name : Text_Type := Selector.To_Text;
   begin
      if Language_Entity_Type (An_Entity.all).Push_Match_Result (Selector, Params) then
         return True;
      end if;

      if Name = "origin" then
         if Params.Children_Count = 0 then
            --  We just checked for the existence of this var and
            --  disregard the actual value

            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            Push_Implicit_Self (An_Entity.Origin);

            Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

            Result := Top_Frame.Data_Stack.Last_Element;
            Top_Frame.Data_Stack.Delete_Last (2); -- unstack origin and result

            if Result = Match_False then
               Push_Match_False;
            else
               Push_Match_True (An_Entity);
            end if;

            return True;
         else
            Error ("only one parameter expected");
         end if;
      elsif An_Entity.Template.Children_Indexed.Contains (Name) then
         -- If the name is a template variable, match the argument
         A_Named_Entity := Named_Entity
           (An_Entity.Template.Children_Indexed.Element (Name));

         if A_Named_Entity.all in Var_Type then
            --  We are calling to match a given parameter value. The convention
            --  is first to stack the name of the variable, then the pattern
            --  (the arguments).

            if Params.Children_Count = 0 then
               --  We just checked for the existence of this var and
               --  disregard the actual value

               Push_Match_True (An_Entity);
            elsif Params.Children_Count = 1 then
               Match_Variable_Value (A_Named_Entity.Name_Node.Text, Params.Child (1));
            else
               Error ("var matcher only takes one parameter");
            end if;
         end if;

         return True;
      elsif Selector.all in Runtime_Static_Entity_Type'Class then
         declare
            Selected : Semantic.Structure.Entity := Runtime_Static_Entity (Selector).An_Entity;
         begin
            if Selected.all in Semantic.Structure.Template_Type'Class then
               if Instance_Of
                 (An_Entity.Template,
                  Semantic.Structure.Template (Selected))
               then
                  if Params.Children_Count = 0 then
                     Push_Match_True (An_Entity);
                  elsif Params.Children_Count = 1 then
                     Push_Implicit_Self (An_Entity);

                     Evaluate_Expression (Params.Child (1).As_Argument.F_Value);
                     Result := Top_Frame.Data_Stack.Last_Element;
                     Pop_Entity (2);

                     if Result = Match_False then
                        Push_Match_False;
                     else
                        Push_Match_True (An_Entity);
                     end if;
                  else
                     Error ("only one parameter allowed for template match");
                  end if;

                  return True;
               end if;
            end if;
         end;
      end if;

      return False;
   end Push_Match_Result;

   overriding
   procedure Evaluate_Bowse_Functions
     (An_Entity                 : access Template_Instance_Type;
      A_Mode                    : Browse_Mode;
      Match_Expression          : Template_Node;
      Evaluate_Match_Expression : access procedure := null)
   is
      procedure Match_Callback is
         Matched_Entity : Language_Entity;
         Result : Runtime_Object;
      begin
         Matched_Entity :=
           Runtime_Language_Entity (Top_Frame.Data_Stack.Last_Element).Value;

         for T of Matched_Entity.Templates_Ordered loop
            Push_Implicit_Self (T);
            Evaluate_Expression (Match_Expression);

            Result := Top_Frame.Data_Stack.Last_Element;
            Top_Frame.Data_Stack.Delete_Last (2);

            if Result /= Match_False then
               Push_Match_True (Result);
               return;
            end if;
         end loop;

         Push_Match_False;
      end Match_Callback;
   begin
      An_Entity.Origin.Evaluate_Bowse_Functions
        (A_Mode, Match_Expression, Match_Callback'Access);
   end Evaluate_Bowse_Functions;

   overriding
   function To_Text (Object : Runtime_Integer_Type) return Text_Type
   is
   begin
      return Object.Value'Wide_Wide_Image;
   end To_Text;

   overriding
   function To_Text_Expression (Object : access Runtime_Integer_Type) return Runtime_Text_Expression is
   begin
      return new Runtime_Text_Type'(Value => To_Unbounded_Text (Object.Value'Wide_Wide_Image));
   end To_Text_Expression;

   function To_Text (Object : Runtime_Text_Container_Type) return Text_Type is
      Result : Unbounded_Text_Type;
   begin
      for T of Object.Texts loop
         Result := Result & T.To_Text;
      end loop;

      return To_Text (Result);
   end To_Text;

   function To_Text_Expression (Object : access Runtime_Text_Container_Type) return Runtime_Text_Expression is
   begin
      return Runtime_Text_Expression (Object);
   end To_Text_Expression;

   overriding
   function To_Text (Object : Runtime_Text_Type) return Text_Type is
   begin
      return To_Text (Object.Value);
   end To_Text;

   overriding
   function To_Text_Expression (Object : access Runtime_Text_Type) return Runtime_Text_Expression is
   begin
      return Runtime_Text_Expression (Object);
   end To_Text_Expression;

   overriding
   function To_Text (Object : Runtime_Template_Variable_Reference_Type) return Text_Type
   is
   begin
      return "";
   end To_Text;

   overriding
   function To_Text_Expression (Object : access Runtime_Template_Variable_Reference_Type) return Runtime_Text_Expression
   is
   begin
      return Runtime_Text_Expression (Object);
   end To_Text_Expression;

end Wrapping.Runtime.Structure;
