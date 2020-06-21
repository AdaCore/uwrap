with Ada.Containers; use Ada.Containers;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Libtemplatelang.Common; use Libtemplatelang.Common;

package body Wrapping.Runtime.Objects is

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

   generic
      A_Mode : in Browse_Mode;
   procedure Call_Gen_Browse
     (Object : access W_Object_Type'Class;
      Params : Libtemplatelang.Analysis.Argument_List);

   procedure Call_Gen_Browse
     (Object : access W_Object_Type'Class;
      Params : Libtemplatelang.Analysis.Argument_List)
   is
   begin
      if Params.Children_Count = 0 then
         W_Node_Type'Class (Object.all).Evaluate_Bowse_Functions
           (A_Mode, Libtemplatelang.Analysis.No_Template_Node);
      elsif Params.Children_Count = 1 then
         W_Node_Type'Class (Object.all).Evaluate_Bowse_Functions
           (A_Mode, Params.Child (1).As_Argument.F_Value);
      elsif Params.Children_Count > 1 then
         Error ("matcher takes only 1 argument");
      end if;
   end Call_Gen_Browse;

   procedure Call_Browse_Parent is new Call_Gen_Browse (Parent);
   procedure Call_Browse_Child is new Call_Gen_Browse (Child_Breadth);
   procedure Call_Browse_Next is new Call_Gen_Browse (Next);
   procedure Call_Browse_Prev is new Call_Gen_Browse (Prev);
   procedure Call_Browse_Sibling is new Call_Gen_Browse (Sibling);
   procedure Call_Browse_Template is new Call_Gen_Browse (Wrapping.Runtime.Structure.Template);

   procedure Call_Browse_Self
     (Object : access W_Object_Type'Class;
      Params : Libtemplatelang.Analysis.Argument_List)
   is
   begin
      if Params.Children_Count = 0 then
         Push_Match_True (Object);
      elsif Params.Children_Count = 1 then
         declare
            Result : W_Object;
         begin
            Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

            Result := Pop_Object;

            if Result /= Match_False then
               Push_Match_True (Object);
            else
               Push_Match_False;
            end if;
         end;
      else
         Error ("self only takes 1 argument");
      end if;
   end Call_Browse_Self;

   procedure Call_Tmp
     (Object : access W_Object_Type'Class;
      Params : Libtemplatelang.Analysis.Argument_List)
   is
   begin
      if Params.Children_Count = 0 then
         Push_Temporary_Name
           ("",
            W_Node (Object).Tmp_Counter);
      elsif Params.Children_Count = 1 then
         Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

         Push_Temporary_Name
           (Pop_Object.To_String,
            W_Node (Object).Tmp_Counter);
      else
         Error ("tmp only accepts one argument");
      end if;
   end Call_Tmp;

   procedure Call_Insert
     (Object : access W_Object_Type'Class;
      Params : Libtemplatelang.Analysis.Argument_List)
   is
      P1, P2 : W_Object;
   begin
      if Params.Children_Count /= 2 then
         Error ("two parameters expected for insert");
      end if;

      P1 := Evaluate_Expression (Params.Child (1).As_Argument.F_Value);
      P2 := Evaluate_Expression (Params.Child (2).As_Argument.F_Value);

      W_Map (Object).A_Map.Insert (P1.To_String, P2);

      Push_Object (W_Object (Object));
   end Call_Insert;

   procedure Call_Include
     (Object : access W_Object_Type'Class;
      Params : Libtemplatelang.Analysis.Argument_List)
   is
      P1, P2 : W_Object;
   begin
      if Params.Children_Count /= 2 then
         Error ("two parameters expected for insert");
      end if;

      P1 := Evaluate_Expression (Params.Child (1).As_Argument.F_Value);
      P2 := Evaluate_Expression (Params.Child (2).As_Argument.F_Value);

      W_Map (Object).A_Map.Include (P1.To_String, P2);

      Push_Object (W_Object (Object));
   end Call_Include;

   procedure Call_Element
     (Object : access W_Object_Type'Class;
      Params : Libtemplatelang.Analysis.Argument_List)
   is
   begin
      if Params.Children_Count /= 1 then
         Error ("element expects one parameter");
      end if;

      declare
         Name : Text_Type := Evaluate_Expression
           (Params.Child (1).As_Argument.F_Value).To_String;
      begin
         if W_Map (Object).A_Map.Contains (Name) then
            Push_Object (W_Map (Object).A_Map.Element (Name));
         elsif Top_Frame.Top_Context.Is_Matching_Context then
            Push_Match_False;
         else
            Error ("map doesn't contain element of name " & Name);
         end if;
      end;
   end Call_Element;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Reference_Type;
      Params    : Libtemplatelang.Analysis.Argument_List)
   is
   begin
      An_Entity.Value.Push_Call_Result (Params);
   end Push_Call_Result;

   overriding
   function Traverse
     (An_Entity    : access W_Reference_Type;
      A_Mode       : Browse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action)
      return Visit_Action
   is
   begin
      return An_Entity.Value.Traverse
        (A_Mode       => A_Mode,
         Include_Self => Include_Self,
         Final_Result => Final_Result,
         Visitor      => Visitor);
   end Traverse;

   overriding
   procedure Evaluate_Bowse_Functions
     (An_Entity        : access W_Reference_Type;
      A_Mode           : Browse_Mode;
      Match_Expression : Template_Node'Class)
   is
   begin
      An_Entity.Value.Evaluate_Bowse_Functions
        (A_Mode           => A_Mode,
         Match_Expression => Match_Expression);
   end Evaluate_Bowse_Functions;

   overriding
   function Browse_Entity
     (An_Entity : access W_Reference_Type;
      Browsed : access W_Object_Type'Class;
      Match_Expression : Template_Node'Class;
      Result : out W_Object) return Visit_Action is
   begin
      return An_Entity.Value.Browse_Entity
        (Browsed          => Browsed,
         Match_Expression => Match_Expression,
         Result           => Result);
   end Browse_Entity;

   function Is_Text_Container (Container : W_Vector_Type) return Boolean is
   begin
      for I of Container.A_Vector loop
         if I.all in W_Vector_Type'Class then
            if not W_Vector (I).Is_Text_Container then
                 return False;
            end if;
         elsif I.all not in W_Text_Expression_Type'Class then
            return False;
         end if;
      end loop;

      return True;
   end Is_Text_Container;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Vector_Type;
      Params    : Libtemplatelang.Analysis.Argument_List)
   is
      Result : W_Object;
   begin
      --  TODO: This will essentially enable checks against strings, which is
      --  useful when vector indeed represent strings. Verify if
      --  this is OK. We may need a specific vector string type for this, and
      --  have a more comprehensive test here.

      if Params.Children_Count = 0 then
         Push_Match_True (An_Entity);
      elsif Params.Children_Count = 1 then
         Push_Implicit_Self (An_Entity);
         Evaluate_Expression (Params.Child (1).As_Argument.F_Value);
         Result := Pop_Object;
         Pop_Object;

         if Result /= Match_False then
            Push_Match_True (An_Entity);
         else
            Push_Match_False;
         end if;
      end if;
   end Push_Call_Result;

   function To_String (Object : W_Vector_Type) return Text_Type is
      Result : Unbounded_Text_Type;
   begin
      for T of Object.A_Vector loop
         Result := Result & (if T /= null then T.To_String else "");
      end loop;

      return To_Text (Result);
   end To_String;

   overriding
   function Push_Value
     (An_Entity : access W_Map_Type;
      Name      : Text_Type) return Boolean
   is
      Call : Call_Access;
   begin
      if Name = "insert" then
         Call := Call_Insert'Access;
      elsif Name = "include" then
         Call := Call_Include'Access;
      elsif Name = "element" then
         Call := Call_Element'Access;
      end if;

      if Call /= null then
         Push_Object
           (W_Object'(new W_Function_Type'
              (Prefix => W_Object (An_Entity),
               Call => Call)));
         return True;
      else
         return False;
      end if;
   end Push_Value;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Map_Type;
      Params    : Libtemplatelang.Analysis.Argument_List)
   is
      Result : W_Object;
      Action : Visit_Action;
   begin
      --  Maps are browsing objects, they behave like various other tree browsing
      --  functions.
      --  TODO: there is no support for object allocation here at this stage,
      --  which is not completely obvious (needs to implement some kind of a
      --  tuple to create key and value at the same time).

      if Params.Children_Count /= 1 then
         Error ("expected one argument for browsing map");
      end if;

      for E of An_Entity.A_Map loop
         Action :=
           An_Entity.Browse_Entity (E, Params.Child (1).As_Argument.F_Value, Result);

         exit when Action = Stop;
      end loop;

      if Result /= null then
         Push_Object (Result);
      else
         Push_Match_False;
      end if;
   end Push_Call_Result;

   overriding
   function To_String (Object : W_Integer_Type) return Text_Type
   is
   begin
      return Object.Value'Wide_Wide_Image;
   end To_String;

   overriding
   function To_String (Object : W_String_Type) return Text_Type is
   begin
      return To_Text (Object.Value);
   end To_String;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_String_Type;
      Params    : Libtemplatelang.Analysis.Argument_List)
   is
      Matched : Boolean;
   begin
      if Params.Children_Count /= 1 then
         Error ("string comparison takes one argument");
      end if;

      Evaluate_Expression (Params.Child (1).As_Argument.F_Value);

      Matched := Runtime.Analysis.Match
        (Pop_Object.To_String, To_Text (An_Entity.Value));

      if Matched then
         Push_Match_True (An_Entity);
      else
         Push_Match_False;
      end if;
   end Push_Call_Result;

   overriding
   function To_String (Object : W_Text_Conversion_Type) return Text_Type is
   begin
      return Object.An_Object.To_String;
   end To_String;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Function_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) is
   begin
      An_Entity.Call (An_Entity.Prefix, Params);
   end Push_Call_Result;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Static_Entity_Type;
      Params    : Libtemplatelang.Analysis.Argument_List)
   is
      Implicit_Self : W_Object;
      Prefix        : W_Template_Instance;
      Result        : W_Object;
   begin
      --  Matching an static entity reference means two things:
      --    *  First, check that this entity reference exist in the context,
      --       that is it's of a subtype of the enclosing self.
      --    *  Second, check that the expression, if any, corresponds to the
      --       components of the enclosing self

      Implicit_Self := Get_Implicit_Self;

      --  This function currently only operates on template instances and
      --  template types. Check that self is a template of the right type.

      if Implicit_Self.all not in W_Template_Instance_Type then
         Push_Match_False;
         return;
      end if;

      Prefix := W_Template_Instance (Implicit_Self);

      if not Instance_Of
        (Prefix.Template,
         Wrapping.Semantic.Structure.Template (An_Entity.An_Entity))
      then
         Push_Match_False;
         return;
      end if;

      --  If we're of the right type, then push the implicit self so that
      --  the stack starts with an implicit entity at the top, and check
      --  the result.

      if Params.Children_Count = 0 then
         Push_Match_True (Implicit_Self);
         return;
      elsif Params.Children_Count = 1 then
         Push_Implicit_Self (Implicit_Self);
         Evaluate_Expression (Params.Child (1).As_Argument.F_Value);
         Result := Pop_Object;
         Pop_Object;

         if Result /= Match_False then
            --  If the result is good, then the result of this match is the
            --  matched object.

            Push_Match_True (Implicit_Self);
            return;
         else
            Push_Match_False;
            return;
         end if;
      else
         Error ("matching a static entity requires one parameter at most");
      end if;
   end Push_Call_Result;

   overriding
   function To_String (Object : W_Lambda_Type) return Text_Type
   is
   begin
      Run_Lambda (Object);

      return Pop_Object.To_String;
   end To_String;

   procedure Add_Child (Parent, Child : access W_Node_Type'Class) is
   begin
      Child.Parent := W_Node (Parent);

      if Parent.Children_Ordered.Length > 0 then
         Parent.Children_Ordered.Last_Element.Next := W_Node (Child);
         Child.Prev := Parent.Children_Ordered.Last_Element;
      end if;

      Parent.Children_Ordered.Append (W_Node (Child));
   end Add_Child;

   procedure Add_Child (Parent, Child : access W_Node_Type'Class; Name : Text_Type) is
   begin
      Add_Child (Parent, Child);
      Parent.Children_Indexed.Insert (Name, W_Node (Child));
   end Add_Child;

   function Create_Template_Instance
     (An_Entity : access W_Node_Type'Class;
      A_Template : Semantic.Structure.Template) return W_Template_Instance
   is
      New_Template : W_Template_Instance;
   begin
      New_Template := new W_Template_Instance_Type;
      New_Template.Template := A_Template;

      if An_Entity /= null then
         New_Template.Origin := W_Node (An_Entity);

         An_Entity.Templates_By_Name.Insert (A_Template.Name_Node.Text, New_Template);
         An_Entity.Templates_By_Full_Id.Insert (A_Template.Full_Name, New_Template);
         An_Entity.Templates_Ordered.Append (New_Template);
      end if;

      Templates_To_Traverse.Append (New_Template);

      return New_Template;
   end Create_Template_Instance;

   function Get_Template_Instance
     (An_Entity : access W_Node_Type'Class;
      Name      : Text_Type) return W_Template_Instance
   is
   begin
      if An_Entity.Templates_By_Name.Contains (Name) then
         return An_Entity.Templates_By_Name.Element (Name);
      else
         return null;
      end if;
   end Get_Template_Instance;

   function Get_Template_Instance
     (An_Entity  : access W_Node_Type'Class;
      A_Template : Semantic.Structure.Template) return W_Template_Instance is
   begin
      --  TODO: These calls to full name may be very costly, it'd be better
      --  to cache the full name in the object
      if An_Entity.Templates_By_Full_Id.Contains (A_Template.Full_Name) then
         return An_Entity.Templates_By_Full_Id.Element (A_Template.Full_Name);
      else
         return null;
      end if;
   end Get_Template_Instance;

   overriding
   function Push_Value
     (An_Entity : access W_Node_Type;
      Name      : Text_Type) return Boolean
   is
      A_Call : Call_Access := null;
   begin
      if An_Entity.Templates_By_Name.Contains (Name) then
         Push_Object (An_Entity.Templates_By_Name.Element (Name));

         return True;
      elsif Name = "parent" then
         A_Call := Call_Browse_Parent'Access;
      elsif Name = "child" then
         A_Call := Call_Browse_Child'Access;
      elsif Name = "next" then
         A_Call := Call_Browse_Next'Access;
      elsif Name = "prev" then
         A_Call := Call_Browse_Prev'Access;
      elsif Name = "sibling" then
         A_Call := Call_Browse_Sibling'Access;
      elsif Name = "template" then
         A_Call := Call_Browse_Template'Access;
      elsif Name = "tmp" then
         A_Call := Call_Tmp'Access;
      elsif Name = "self" then
         A_Call := Call_Browse_Self'Access;
      end if;

      if A_Call /= null then
         Push_Object
           (W_Object'(new W_Function_Type'
                (Prefix => W_Object (An_Entity),
                 Call => A_Call)));

         return True;
      end if;

      return False;
   end Push_Value;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Node_Type;
      Params    : Argument_List)
   is
      Result : W_Object;
   begin
      if Params.Children_Count = 0 then
         Push_Match_True (An_Entity);
      elsif Params.Children_Count = 1 then
         Push_Implicit_Self (W_Object (An_Entity));
         Evaluate_Expression (Params.Child (1).As_Argument.F_Value);
         Result := Pop_Object;
         Pop_Object;

         if Result = Match_False then
            Push_Match_False;
         else
            Push_Match_True (An_Entity);
         end if;
      else
         Error ("comparing with a node requires one parameter");
      end if;
   end Push_Call_Result;

   function Traverse
     (An_Entity    : access W_Node_Type;
      A_Mode       : Browse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action)
      return Visit_Action
   is
      Current : W_Node;
      Current_Children_List : W_Node_Vectors.Vector;
      Next_Children_List : W_Node_Vectors.Vector;
   begin
      W_Node_Type'Class (An_Entity.all).Pre_Visit;

      if Include_Self then
         W_Node_Type'Class (An_Entity.all).Pre_Visit;

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
         case W_Node_Type'Class (An_Entity.all).Traverse
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

         return W_Node_Type'Class (An_Entity.all).Traverse
           (Next,
            False,
            Final_Result,
            Visitor);
      elsif A_Mode = Wrapping.Runtime.Structure.Template then
         for T of An_Entity.Templates_Ordered loop
            W_Node_Type'Class (T.all).Pre_Visit;

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

         when Sibling | Wrapping.Runtime.Structure.Template =>
            null;

      end case;

      if A_Mode = Child_Breadth then
         loop
            for C of Current_Children_List loop
               W_Node_Type'Class (C.all).Pre_Visit;

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
            W_Node_Type'Class (Current.all).Pre_Visit;

            case Visitor (Current, Final_Result) is
               when Stop =>
                  return Stop;

               when Over =>
                  null;

               when Into =>
                  if A_Mode = Child_Depth then
                     case W_Node_Type'Class
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

               when Sibling | Wrapping.Runtime.Structure.Template =>
                  null;

            end case;
         end loop;
      end if;

      return Into;
   end Traverse;

    procedure Evaluate_Bowse_Functions
     (An_Entity         : access W_Node_Type;
      A_Mode            : Browse_Mode;
      Match_Expression  : Template_Node'Class)
   is
      function Visitor
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action is
      begin
         return Browse_Entity (An_Entity, E, Match_Expression, Result);
      end Visitor;

      procedure Allocate (E : access W_Object_Type'Class) is
      begin
         --  TODO: We need to be able to cancel allocation if the entire
         --  research happens to be false

         case A_Mode is
            when Child_Depth | Child_Breadth =>
               Add_Child (W_Node (An_Entity), W_Node (E));

            when others =>
               Error ("allocation not implemented on the enclosing function");
         end case;
      end Allocate;

      Found : Boolean;
      Result : W_Object;
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.An_Allocate_Callback := null;
      Top_Frame.Top_Context.Is_Matching_Context := True;

      Found := W_Node_Type'Class(An_Entity.all).Traverse
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

         Found := W_Node_Type'Class(An_Entity.all).Traverse
           (A_Mode, False, Result, Visitor'Access) = Stop;

         if not Found then
            --  If still not found, there is still a possibilty that this can
            --  match without any object valid, and then create the first element.

            declare
               Dummy_Entity : W_Node;
            begin
               Dummy_Entity := new W_Node_Type;

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

   procedure Print (An_Entity : W_Node_Type; Indent : Text_Type := "") is
   begin
      Put_Line (Indent & An_Entity.To_String);

      for E of An_Entity.Children_Ordered loop
         Print (E.all, Indent & "-");
      end loop;
   end Print;

   function Push_Value
     (An_Entity : access W_Template_Instance_Type;
      Name      : Text_Type) return Boolean
   is
      use Wrapping.Semantic.Structure;

      Named_Entity : Entity;
      Result : W_Object;
   begin
      if W_Node_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      -- First cover the case of a variable or a pattern

      if Name = "origin" then
         if An_Entity.Origin /= null then
            Push_Object (An_Entity.Origin);

            return True;
         end if;
      elsif An_Entity.Symbols.Contains (Name) then
         Push_Object (An_Entity.Symbols.Element (Name));
         return True;
      elsif An_Entity.Template /= null then
         --  If we did not find the symbol, see if it corresponds to a variable
         --  and create it.

         Named_Entity := An_Entity.Template.Get_Component (Name);

         if Named_Entity /= null then
            if Named_Entity.all in Var_Type then
               declare
                  A_Var : Semantic.Structure.Var :=  Semantic.Structure.Var (Named_Entity);
                  New_Ref : W_Reference;
               begin
                  if A_Var.Kind = Text_Kind then

                     --  Symbols contained in templates are references to
                     --  values. Create the reference and the referenced
                     --  empty value here.

                     New_Ref := new W_Reference_Type;
                     New_Ref.Value := new W_Vector_Type;

                     An_Entity.Symbols.Insert (Name, New_Ref);

                     Push_Object (An_Entity.Symbols.Element (Name));

                     return True;
                  elsif A_Var.Kind = Pattern_Kind then
                     --  If it's a pattern, just return the current value of the result
                     --  as a text expression. This will need to be evaluated, push
                     --  self on the stack

                     Push_Object (An_Entity);

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

   overriding
   function Traverse
     (An_Entity    : access W_Template_Instance_Type;
      A_Mode       : Browse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action)
      return Visit_Action
   is
      function Template_Visitor
        (E      : access W_Object_Type'Class;
         Result : out W_Object)
         return Visit_Action
      is
         Last_Result : Visit_Action := Into;
      begin
         if E.all in W_Node_Type'Class then
            for T of W_Node (E).Templates_Ordered loop
               Last_Result := Visitor (T, Result);

               if Last_Result = Stop then
                  return Stop;
               end if;
            end loop;
         end if;

         return Last_Result;
      end Template_Visitor;

      Last_Result : Visit_Action := Into;
   begin
      --  First, traverse through the actual structure of the template.

      if Last_Result /= Stop then
         --  Call to the super traverse function
         Last_Result := W_Node_Type (An_Entity.all).Traverse
           (A_Mode, Include_Self, Final_Result, Visitor);
      end if;

      --  Then, try to traverse the template implicit structure held by the
      --  original node.

      if Last_Result /= Stop and then An_Entity.Origin /= null then
         Last_Result := An_Entity.Origin.Traverse
           (A_Mode, False, Final_Result, Template_Visitor'Access);
      end if;
      --
      return Last_Result;
   end Traverse;

end Wrapping.Runtime.Objects;
