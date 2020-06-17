with Ada.Containers; use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;
with Libtemplatelang.Common; use Libtemplatelang.Common;
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

   function Push_Value
     (An_Entity : access Language_Entity_Type;
      Name      : Text_Type) return Boolean
   is
   begin
      if An_Entity.Templates_By_Name.Contains (Name) then
         Push_Entity (An_Entity.Templates_By_Name.Element (Name));

         return True;
      elsif Name = "parent"
        or else Name = "child"
        or else Name = "next"
        or else Name = "prev"
        or else Name = "sibling"
        or else Name = "template"
        or else Name = "tmp"
      then
         Push_Object
           (Runtime_Object'(new Runtime_Function_Reference_Type'
                (Name   => To_Unbounded_Text (Name),
                 Prefix => Language_Entity (An_Entity))));

         return True;
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
      elsif Name = "child" then
         A_Mode := Child_Breadth;
      elsif Name = "prev" then
         A_Mode := Prev;
      elsif Name = "next" then
         A_Mode := Next;
      elsif Name = "sibling" then
         A_Mode := Sibling;
      elsif Name = "template" then
          A_Mode := Template;
      elsif Name = "self" then
         if Params.Children_Count = 0 then
            Push_Match_True (An_Entity);
         elsif Params.Children_Count = 1 then
            declare
               Result : Runtime_Object;
            begin
               Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

               Result := Pop_Object;

               if Result /= Match_False then
                  Push_Match_True (An_Entity);
               else
                  Push_Match_False;
               end if;
            end;
         else
            Error ("self only takes 1 argument");
         end if;

         return True;
      else
         return False;
      end if;

      if Params.Children_Count = 0 then
         Language_Entity_Type'Class (An_Entity.all).Evaluate_Bowse_Functions
           (A_Mode, Libtemplatelang.Analysis.No_Template_Node);
      elsif Params.Children_Count = 1 then
         Language_Entity_Type'Class (An_Entity.all).Evaluate_Bowse_Functions
           (A_Mode, Params.Child (1).As_Argument.F_Value);
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
      if Push_Browse_Result (An_Entity, Name, Params) then
         return True;
      elsif Name = "tmp" then
         if Params.Children_Count = 0 then
            Push_Temporary_Name
              ("",
               An_Entity.Tmp_Counter);
         elsif Params.Children_Count = 1 then
            Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

            Push_Temporary_Name
              (Pop_Object.To_Text,
               An_Entity.Tmp_Counter);
         else
            Error ("tmp only accepts one argument");
         end if;

         return True;
      else
         return False;
      end if;
   end Push_Call_Result;

   function Push_Match_Result
     (An_Entity : access Language_Entity_Type;
      Selector  : Runtime_Object;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean
   is
   begin
      return Language_Entity_Type'Class
        (An_Entity.all).Push_Browse_Result (Selector.To_Text, Params);
   end Push_Match_Result;

   function Traverse
     (An_Entity    : access Language_Entity_Type;
      A_Mode       : Browse_Mode;
      Include_Self : Boolean;
      Final_Result : out Runtime_Object;
      Visitor      : access function
        (E      : access Language_Entity_Type'Class;
         Result : out Runtime_Object) return Visit_Action)
      return Visit_Action
   is
      Current : Language_Entity;
      Current_Children_List : Language_Entity_Vectors.Vector;
      Next_Children_List : Language_Entity_Vectors.Vector;
   begin
      Language_Entity_Type'Class (An_Entity.all).Pre_Visit;

      if Include_Self then
         Language_Entity_Type'Class (An_Entity.all).Pre_Visit;

         case Visitor (An_Entity, Final_Result) is
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
            Final_Result,
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
            Final_Result,
            Visitor);
      elsif A_Mode = Template then
         for T of An_Entity.Templates_Ordered loop
            Language_Entity_Type'Class (T.all).Pre_Visit;

            case Visitor (T, Final_Result) is
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

               case Visitor (C, Final_Result) is
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

            case Visitor (Current, Final_Result) is
               when Stop =>
                  return Stop;

               when Over =>
                  null;

               when Into =>
                  if A_Mode = Child_Depth then
                     case Language_Entity_Type'Class
                       (Current.all).Traverse (A_Mode, False, Final_Result, Visitor)
                     is
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


   function Has_Allocator (Node : Template_Node'Class) return Boolean is
      Found : Boolean := False;

      function Visit (Node : Template_Node'Class) return Visit_Status is
      begin
         if Node.Kind = Template_New_Expr then
            Found := True;
            return Stop;
         else
            return Into;
         end if;
      end Visit;
   begin
      Node.Traverse (Visit'Access);

      return Found;
   end Has_Allocator;

   procedure Evaluate_Bowse_Functions
     (An_Entity         : access Language_Entity_Type;
      A_Mode            : Browse_Mode;
      Match_Expression  : Template_Node'Class)
   is
      function Visitor
        (E      : access Language_Entity_Type'Class;
         Result : out Runtime_Object) return Visit_Action is
      begin
         return Browse_Entity (An_Entity, E, Match_Expression, Result);
      end Visitor;

      procedure Allocate (E : access Language_Entity_Type'Class) is
      begin
         --  TODO: We need to be able to cancel allocation if the entire
         --  research happens to be false

         case A_Mode is
            when Child_Depth | Child_Breadth =>
               Add_Child (Language_Entity (An_Entity), Language_Entity (E));

            when others =>
               Error ("allocation not implemented on the enclosing function");
         end case;
      end Allocate;

      Found : Boolean;
      Result : Runtime_Object;
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.An_Allocate_Callback := null;
      Top_Frame.Top_Context.Is_Matching_Context := True;

      Found := Language_Entity_Type'Class(An_Entity.all).Traverse
        (A_Mode, False, Result, Visitor'Access) = Stop;

      if not Found and Has_Allocator (Match_Expression) then
         --  Semantic for search is to look first for matches that do not require
         --  an allocator. If none is found and if there are allocators, then
         --  re-try, this time with allocators enabled.

         if Top_Frame.Top_Context.Is_Folding_Context then
            --  TODO: it would be best to check that earlier in the system,
            --  as opposed to only when trying to call a folding function.
            Error ("allocators are not allowed in folding browsing functions");
         end if;

         Top_Frame.Top_Context.An_Allocate_Callback := Allocate'Unrestricted_Access;

         Found := Language_Entity_Type'Class(An_Entity.all).Traverse
           (A_Mode, False, Result, Visitor'Access) = Stop;

         if not Found then
            --  If still not found, there is still a possibilty that this can
            --  match without any object valid, and then create the first element.

            declare
               Dummy_Entity : Language_Entity;
            begin
               Dummy_Entity := new Language_Entity_Type;

               Found := Visitor (Dummy_Entity, Result) = Stop;
            end;
         end if;
      end if;

      Pop_Frame_Context;

      if not Found and then
        not
          (Top_Frame.Top_Context.Is_Matching_Context
           or else Top_Frame.Top_Context.Is_Folding_Context)
      then
         Error ("no result found for browsing function");
      end if;

      if Result /= null then
         Push_Object (Result);
      else
         Push_Match_False;
      end if;
   end Evaluate_Bowse_Functions;

   function Browse_Entity
     (An_Entity : access Language_Entity_Type'Class;
      Browsed : access Language_Entity_Type'Class;
      Match_Expression : Template_Node'Class;
      Result : out Runtime_Object) return Visit_Action
   is
      procedure Evaluate_Fold_Function is
      begin
         --  When evaluating a folding function in a browsing call, we need to
         --  first deactivate folding in the expression itself. We also we need
         --  to remove potential name capture, as it would override the one we
         --  are capturing in this browsing iteration.

         Push_Frame_Context;
         Top_Frame.Top_Context.Is_Folding_Context := False;
         Top_Frame.Top_Context.Is_Matching_Context := False;
         Top_Frame.Top_Context.Name_Captured := To_Unbounded_Text ("");

         --  Then evaluate that folding expression

         Push_Implicit_Self (Browsed);
         Evaluate_Expression (Top_Frame.Top_Context.Folding_Expression);

         --  The result of the evaluate expression is the result of the
         --  folding function, as opposed to the matching entity in normal
         --  browsing.
         Result := Pop_Object;
         Pop_Object;

         --  Pop frame context. This will in particular restore the name
         --  catpure, which we're using as the accumulator.
         Pop_Frame_Context;

         --  If there's an name to store the result, store it there.

         if Top_Frame.Top_Context.Name_Captured /= "" then
            Top_Frame.Symbols.Include
              (To_Text (Top_Frame.Top_Context.Name_Captured),
               Result);
         end if;
      end Evaluate_Fold_Function;

      Expression_Result : Runtime_Object;

   begin
      Result := null;

      --  If the match expression is null, we're only looking for the
      --  presence of a node, not its form. The result is always true.
      if Match_Expression.Is_Null then
         if Top_Frame.Top_Context.Is_Folding_Context then
            Evaluate_Fold_Function;

            return Into;
         else
            Result := new Runtime_Language_Entity_Type'
              (Value => Language_Entity (Browsed), others => <>);

            return Stop;
         end if;
      end if;

      --  There is a subtetly in the browsing functions. The self reference
      --  within these calls isn't the entity currently analyzed anymore but
      --  directly the entity that is being evaluated under these calls.
      --  However, we cannot create a sub frame as whatever we match needs
      --  to find its way to the command frame (otherwise any extracted
      --  group would be deleted upon frame popped).
      --  TODO: these specificities needs to be duly documented in the UG.
      Push_Implicit_Self (Browsed);

      --  If there's a name capture above this expression, its value needs
      --  to be available in the underlying match expression. We only capture
      --  the entity outside of folding context. When folding, the result of
      --  the folding expression will actually be what needs to be captured.

      if Top_Frame.Top_Context.Name_Captured /= ""
        and then not Top_Frame.Top_Context.Is_Folding_Context
      then
         Top_Frame.Symbols.Include
           (To_Text (Top_Frame.Top_Context.Name_Captured),
            new Runtime_Language_Entity_Type'
              (Value => Language_Entity (Browsed), others => <>));
      end if;

      --  Prior to evaluating the expression, we need to remove potential name
      --  capture, as it would override the one we are capturing in this browsing
      --  iteration.

      Push_Frame_Context;
      Top_Frame.Top_Context.Name_Captured := To_Unbounded_Text ("");

      Evaluate_Expression (Match_Expression);

      Pop_Frame_Context;

      Expression_Result := Pop_Object;

      Pop_Object;

      if Expression_Result /= Match_False then
         if Expression_Result.all in Runtime_Language_Entity_Type'Class
           and then Runtime_Language_Entity (Expression_Result).Is_Allocated
         then
            --  If we have a result only in allocated mode, and if this result
            --  is a new entity, this means that this new entity is actually
            --  the result of the browse, not the one searched.

            Result := Expression_Result;

            --  Note that it is illegal to call a fold function with an
            --  allocator in the fold expression (we would never know when to
            --  stop allocating). This case is supposed to have being taken
            --  care of earlier but raise an error here just in case.

            if Top_Frame.Top_Context.Is_Folding_Context then
               Error ("allocation in folding browsing functions is illegal");
            end if;

            return Stop;
         else
            if Top_Frame.Top_Context.Is_Folding_Context then
               Evaluate_Fold_Function;

               return Into;
            else
               Result := new Runtime_Language_Entity_Type'
                 (Value => Language_Entity (Browsed), others => <>);

               return Stop;
            end if;
         end if;
      else
         return Into;
      end if;
   end Browse_Entity;

   procedure Push_Match_True (An_Entity : access Language_Entity_Type) is
   begin
      Push_Entity (An_Entity);
   end Push_Match_True;

   procedure Push_Match_True (An_Entity : access Runtime_Object_Type'Class) is
   begin
      Push_Object (An_Entity);
   end Push_Match_True;

   procedure Push_Match_False is
   begin
      Push_Object (Match_False);
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

      if An_Entity /= null then
         New_Template.Origin := Language_Entity (An_Entity);

         An_Entity.Templates_By_Name.Insert (A_Template.Name_Node.Text, New_Template);
         An_Entity.Templates_By_Full_Id.Insert (A_Template.Full_Name, New_Template);
         An_Entity.Templates_Ordered.Append (New_Template);
      end if;

      Templates_To_Traverse.Append (New_Template);

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
                     if not Top_Frame.Top_Context.Is_Matching_Context then
                        if not An_Entity.Symbols.Contains (Name) then
                           An_Entity.Symbols.Insert (Name, new Runtime_Text_Container_Type);
                        end if;

                        Push_Object (An_Entity.Symbols.Element (Name));

                        return True;
                     else
                        Push_Object
                          (Runtime_Object'(new Runtime_Function_Reference_Type'
                               (Name   => To_Unbounded_Text (Name),
                                Prefix => Language_Entity (An_Entity))));

                        return True;
                     end if;
                  elsif A_Var.Kind = Pattern_Kind then
                     --  If it's a pattern, just return the current value of the result
                     --  as a text expression. This will need to be evaluated, push
                     --  self on the stack

                     Push_Entity (An_Entity);

                     Evaluate_Expression (A_Var.Args.Child (1).As_Argument.F_Value);

                     Result := Pop_Object;
                     Pop_Object;

                     Push_Object (Result);

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
      if Language_Entity_Type (An_Entity.all).Push_Call_Result (Name, Params) then
         return True;
      end if;

      return False;
   end Push_Call_Result;

   function Push_Match_Result
     (An_Entity : access Template_Instance_Type;
      Selector  : Runtime_Object;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean
   is
      Dummy_Boolean : Boolean;

      use Wrapping.Semantic.Structure;

      procedure Match_Variable_Value (Name : Text_Type; Expression : Libtemplatelang.Analysis.Template_Node'Class) is
         Result : Runtime_Object;
         String_Entity : Language_Entity;
      begin
         if An_Entity.Symbols.Contains (Name) then
            String_Entity := new String_Langage_Entity_Type'
              (Value => To_Unbounded_Text (An_Entity.Symbols.Element (Name).To_Text), others => <>);
         else
            Push_Match_False;
            return;
         end if;

         Push_Implicit_Self (String_Entity);

         Evaluate_Expression (Expression);

         Result := Pop_Object;
         Pop_Object;

         if Result /= Match_False then
            Dummy_Boolean := An_Entity.Push_Value (Name);
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
         if An_Entity.Origin = null then
            Push_Match_False;
            return True;
         end if;

         if Params.Children_Count = 0 then
            --  We just checked for the existence of this var and
            --  disregard the actual value

            Push_Match_True (An_Entity.Origin);

            return True;
         elsif Params.Children_Count = 1 then
            Push_Implicit_Self (An_Entity.Origin);

            Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

            -- unstack origin and result
            Result := Pop_Object;
            Pop_Object;

            if Result = Match_False then
               Push_Match_False;
            else
               Push_Match_True (An_Entity.Origin);
            end if;

            return True;
         else
            Error ("only one parameter expected");
         end if;
      elsif An_Entity.Template.Get_Component (Name) /= null then
         -- If the name is a template variable, match the argument
         A_Named_Entity := Named_Entity
           (An_Entity.Template.Get_Component (Name));

         if A_Named_Entity.all in Var_Type then
            --  We are calling to match a given parameter value. The convention
            --  is first to stack the name of the variable, then the pattern
            --  (the arguments).

            if Params.Children_Count = 0 then
               --  We just checked for the existence of this var and
               --  disregard the actual value

               Dummy_Boolean := An_Entity.Push_Value (Name);
            elsif Params.Children_Count = 1 then
               Match_Variable_Value (A_Named_Entity.Name_Node.Text, Params.Child (1).As_Argument.F_Value);
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
                  --  Put_Line ("******************* TRUE");
                  if Params.Children_Count = 0 Then
                     Push_Match_True (An_Entity);
                  elsif Params.Children_Count = 1 then
                     Push_Implicit_Self (An_Entity);

                     Evaluate_Expression (Params.Child (1).As_Argument.F_Value);
                     Result := Pop_Object;
                     Pop_Object;

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
      Match_Expression          : Template_Node'Class)
   is
      function Template_Visitor
        (E      : access Language_Entity_Type'Class;
         Result : out Runtime_Object)
         return Visit_Action
      is
      begin
         for T of E.Templates_Ordered loop
            if Browse_Entity (An_Entity, T, Match_Expression, Result) = Stop then
               return Stop;
            end if;
         end loop;

         return Into;
      end Template_Visitor;

      function Self_Visitor
        (E : access Language_Entity_Type'Class;
         Result : out Runtime_Object)
         return Visit_Action is
      begin
         return Browse_Entity (An_Entity, E, Match_Expression, Result);
      end Self_Visitor;

      procedure Allocate (E : access Language_Entity_Type'Class) is
      begin
         --  TODO: We need to be able to cancel allocation if the entire
         --  research happens to be false

         case A_Mode is
            when Child_Depth | Child_Breadth =>
               Add_Child (Language_Entity (An_Entity), Language_Entity (E));

            when others =>
               Error ("allocation not implemented on the enclosing function");
         end case;
      end Allocate;

      Result : Runtime_Object := Match_False;

      Found : Boolean;
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.An_Allocate_Callback := null;
      Top_Frame.Top_Context.Is_Matching_Context := True;

      -- The sequence of traversal for template goes as follows:
      -- (1) traverse on the origin, see if there's any matching template there,
      --     as the origin structure is implicitely the structure of the template.
      -- (2) traverse the template, as it can has allocated structure on its own
      -- (3) if there's possibity of an allocation, redo (1) and (2) with
      --     allocation enabled.
      -- (4) finally, try to traverse with a dummy entity with allocation
      --     enabled to check if an empty

      if An_Entity.Origin /= null then
         Found := Language_Entity_Type'Class(An_Entity.Origin.all).Traverse
           (A_Mode, False, Result, Template_Visitor'Access) = Stop;
      end if;

      if not Found then
         Found := Language_Entity_Type'Class(An_Entity.all).Traverse
           (A_Mode, False, Result, Self_Visitor'Access) = Stop;
      end if;

      if not Found and Has_Allocator (Match_Expression) then
         --  Semantic for search is to look first for matches that do not require
         --  an allocator. If none is found and if there are allocators, then
         --  re-try, this time with allocators enabled.

         if Top_Frame.Top_Context.Is_Folding_Context then
            --  TODO: it would be best to check that earlier in the system,
            --  as opposed to only when trying to call a folding function.
            Error ("allocators are not allowed in folding browsing functions");
         end if;

         Top_Frame.Top_Context.An_Allocate_Callback := Allocate'Unrestricted_Access;

         if An_Entity.Origin /= null then
            Found := Language_Entity_Type'Class(An_Entity.Origin.all).Traverse
              (A_Mode, False, Result, Template_Visitor'Access) = Stop;
         end if;

         if not Found then
            Found := Language_Entity_Type'Class(An_Entity.all).Traverse
              (A_Mode, False, Result, Self_Visitor'Access) = Stop;
         end if;

         if not Found then
            --  If still not found, there is still a possibilty that this can
            --  match without any object valid, and then create the first element.

            declare
               Dummy_Entity : Language_Entity;
            begin
               Dummy_Entity := new Language_Entity_Type;

               Found := Self_Visitor (Dummy_Entity, Result) = Stop;
            end;
         end if;
      end if;

      Pop_Frame_Context;

      if not Found and then
        not
          (Top_Frame.Top_Context.Is_Matching_Context
           or else Top_Frame.Top_Context.Is_Folding_Context)
      then
         Error ("no result found for browsing function");
      end if;

      if Result /= null then
         Push_Object (Result);
      else
         Push_Match_False;
      end if;
   end Evaluate_Bowse_Functions;

   overriding
   function Push_Match_Result
     (An_Entity : access String_Langage_Entity_Type;
      Selector  : Runtime_Object;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean
   is
      Matched : Boolean;
   begin
      Matched := Runtime.Analysis.Match (Selector.To_Text, To_Text (An_Entity.Value));

      if Matched then
         Push_Match_True (An_Entity);
      else
         Push_Match_False;
      end if;

      return True;
   end Push_Match_Result;

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
         Result := Result & (if T /= null then T.To_Text else "");
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
   function To_Text (Object : Runtime_Lambda_Type) return Text_Type
   is
   begin
      Run_Lambda (Object);
      return Pop_Object.To_Text;
   end To_Text;

end Wrapping.Runtime.Structure;
