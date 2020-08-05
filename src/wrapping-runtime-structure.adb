with Ada.Containers; use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Tags; use Ada.Tags;
with Ada.Unchecked_Conversion;
with System; use System;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;
with Libtemplatelang.Common; use Libtemplatelang.Common;
with Wrapping.Semantic.Structure;
with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Wrapping.Runtime.Functions; use Wrapping.Runtime.Functions;
with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;

package body Wrapping.Runtime.Structure is

   Root_Language_Entities : W_Node_Maps.Map;

   function Lt_Wrapper (Left, Right : W_Object) return Boolean is
   begin
      return Left.Lt (Right);
   end Lt_Wrapper;

   function Eq_Wrapper (Left, Right : W_Object) return Boolean is
   begin
      return Left.Eq (Right);
   end Eq_Wrapper;

   function Get_Visible_Symbol (A_Frame: Data_Frame_Type; Name : Text_Type) return W_Object is
   begin
      if A_Frame.Symbols.Contains (Name) then
         return A_Frame.Symbols.Element (Name);
      elsif A_Frame.Parent_Frame /= null then
         return Get_Visible_Symbol (A_Frame.Parent_Frame.all, Name);
      else
         return null;
      end if;
   end Get_Visible_Symbol;

   function Get_Module (A_Frame : Data_Frame_Type) return Semantic.Structure.T_Module is
      use Semantic.Structure;

      Scope : Semantic.Structure.T_Entity := A_Frame.Lexical_Scope;
   begin
      while Scope /= null and then Scope.all not in T_Module_Type'Class loop
         Scope := Scope.Parent;
      end loop;

      return Semantic.Structure.T_Module (Scope);
   end Get_Module;

   function Traverse
     (An_Entity    : access W_Object_Type;
      A_Mode       : Browse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action)
      return Visit_Action
   is
   begin
      Final_Result := null;
      return Into;
   end Traverse;

   function Browse_Entity
     (An_Entity : access W_Object_Type;
      Browsed : access W_Object_Type'Class;
      Match_Expression : T_Expr;
      Result : out W_Object) return Visit_Action
   is
      Visit_Decision : aliased Visit_Action := Unknown;

      procedure Evaluate_Expand_Function
        with Post => Top_Frame.Data_Stack.Length =
          Top_Frame.Data_Stack.Length'Old
      is
      begin
         --  In certain cases, there's no expression to be evaluated upon
         --  expansion. E.g.:
         --    x.all ()
         --  as opposed to:
         --    x.all().something().
         if Top_Frame.Top_Context.Expand_Action = null then
            return;
         end if;

         --  When evaluating a expanding function in a browsing call, we need to
         --  first deactivate expanding in the expression itself. We also we need
         --  to remove potential name capture, as it would override the one we
         --  are capturing in this browsing iteration. TODO: quite the opposite if we do fold (i : inti, i: acc);

         Push_Frame_Context;
         Top_Frame.Top_Context.Is_Expanding_Context := False;
         Top_Frame.Top_Context.Match_Mode := Match_None;
         Top_Frame.Top_Context.Name_Captured := To_Unbounded_Text ("");
         Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;
         Top_Frame.Top_Context.Visit_Decision := Visit_Decision'Unchecked_Access;

         --  Then evaluate that folding expression

         Push_Implicit_Self (Browsed);
         Top_Frame.Top_Context.Expand_Action.all;

         --  The result of the evaluate expression is the result of the
         --  expanding function, as opposed to the matching entity in normal
         --  browsing.
         Result := Pop_Object;
         Pop_Object;

         --  Pop frame context. This will in particular restore the name
         --  catpure, which we're using as the accumulator.
         Pop_Frame_Context;

         --  If there's an name to store the result, store it there.

         if Top_Frame.Top_Context.Name_Captured /= "" then
            Include_Symbol
              (To_Text (Top_Frame.Top_Context.Name_Captured),
               Result);
         end if;
      end Evaluate_Expand_Function;

      Expression_Result : W_Object;

   begin
      Result := null;

      --  If the match expression is null, we're only looking for the
      --  presence of a node, not its form. The result is always true.
      if Match_Expression = null then
         --  In the case of
         --     pick child().all(),
         --  child needs to be evaluated against the outer expression to
         --  be captured by the possible wrap or weave command.

         Push_Frame_Context;
         Top_Frame.Top_Context.Visit_Decision := Visit_Decision'Unchecked_Access;

         Push_Implicit_Self (Browsed);
         Top_Frame.Top_Context.Outer_Expr_Callback.all;
         Pop_Object;
         Pop_Frame_Context;

         if Top_Frame.Top_Context.Is_Expanding_Context then
            Evaluate_Expand_Function;

            if Visit_Decision = Unknown then
               return Into;
            else
               return Visit_Decision;
            end if;
         else
            Result := new W_Reference_Type'
              (Value => W_Object (Browsed), others => <>);

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
        and then not Top_Frame.Top_Context.Is_Expanding_Context
      then
         Include_Symbol
           (To_Text (Top_Frame.Top_Context.Name_Captured),
            new W_Reference_Type'
              (Value => W_Object (Browsed), others => <>));
      end if;

      --  Prior to evaluating the expression, we need to remove potential name
      --  capture, as it would override the one we are capturing in this browsing
      --  iteration.

      Push_Frame_Context;
      Top_Frame.Top_Context.Name_Captured := To_Unbounded_Text ("");
      Top_Frame.Top_Context.Outer_Object := W_Object (Browsed);
      Top_Frame.Top_Context.Match_Mode := Match_Ref_Default;
      Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;
      Top_Frame.Top_Context.Visit_Decision := Visit_Decision'Unchecked_Access;

      Evaluate_Expression (Match_Expression);

      Pop_Frame_Context;

      Expression_Result := Pop_Object;

      Pop_Object;

      if Expression_Result /= Match_False then
         if Expression_Result.all in W_Reference_Type'Class
           and then W_Reference (Expression_Result).Is_Allocated
         then
            --  If we have a result only in allocated mode, and if this result
            --  is a new entity, this means that this new entity is actually
            --  the result of the browse, not the one searched.

            Result := Expression_Result;

            --  Note that it is illegal to call a fold function with an
            --  allocator in the fold expression (we would never know when to
            --  stop allocating). This case is supposed to have being taken
            --  care of earlier but raise an error here just in case.

            if Top_Frame.Top_Context.Is_Expanding_Context then
               Error ("allocation in expanding browsing functions is illegal");
            end if;

            return Stop;
         else
            if Top_Frame.Top_Context.Is_Expanding_Context then
               Evaluate_Expand_Function;

               --  The result of the expansion can be calls to wrap functions,
               --  and one of this wrap function may be a visit decision. If
               --  that's the case, take it into account and reset the flag.

               if Visit_Decision = Unknown then
                  return Into;
               else
                  return Visit_Decision;
               end if;
            else
               Result := W_Object (Browsed);

               return Stop;
            end if;
         end if;
      else
         return Into;
      end if;
   end Browse_Entity;

   procedure Push_Match_True (An_Entity : access W_Object_Type'Class) is
   begin
      Push_Object (An_Entity);
   end Push_Match_True;

   procedure Push_Match_False is
   begin
      Push_Object (Match_False);
   end Push_Match_False;

   procedure Include_Symbol (Name : Text_Type; Object : W_Object) is
   begin
      pragma Assert
        (if Object.all in W_Reference_Type'Class
         then W_Reference (Object).Value /= null);

      Top_Frame.Symbols.Include (Name, Object);
   end Include_Symbol;

   procedure Push_Call_Result
     (An_Entity : access W_Object_Type;
      Params    : T_Arg_Vectors.Vector) is
   begin
      Error ("non callable entity");
   end Push_Call_Result;

   function Match_With_Top_Object
     (An_Entity : access W_Object_Type) return Boolean
   is
      Other_Entity : W_Object := Top_Object.Dereference;
      Matched : Boolean;
   begin
      if Other_Entity = Match_False then
         return True;
      elsif Other_Entity.all in W_Regexp_Type'Class then
         Matched := Runtime.Analysis.Match
           (Other_Entity.To_String,
            W_Object_Type'Class (An_Entity.all).To_String);

         if not Matched then
            Pop_Object;
            Push_Match_False;
         end if;

         return True;
      elsif Other_Entity.all in W_Text_Expression_Type'Class then
         if Other_Entity.To_String /= W_Object_Type'Class (An_Entity.all).To_String then
            Pop_Object;
            Push_Match_False;
         end if;

         return True;
      elsif Other_Entity.all in W_Intrinsic_Function_Type'Class then
         --  Functions always match, their result is evaluated later.

         return True;
      end if;

      return False;
   end Match_With_Top_Object;

   function Lt
     (Left : access W_Object_Type; Right : access W_Object_Type'Class)
      return Boolean
   is
      Left_Tag : Tag := W_Object (Left).all'Tag;
      Right_Tag : Tag := Right.all'Tag;

      function To_Address is new Ada.Unchecked_Conversion (Tag, System.Address);
   begin
      if Left_Tag = Right_Tag then
         return Left.all'Address < Right.all'Address;
      else
         return To_Address (Left_Tag) < To_Address (Right_Tag);
      end if;
   end Lt;

   function Eq (Left : access W_Object_Type; Right : access W_Object_Type'Class) return Boolean is
   begin
      return Left = Right;
   end Eq;

   Object_For_Entity_Registry : W_Object_Maps.Map;

   function Get_Object_For_Entity
     (An_Entity : access T_Entity_Type'Class) return W_Object
   is
      Result : W_Template_Instance;
      Name : Text_Type := An_Entity.Full_Name;

      procedure Allocate_Variable (Var : T_Var) is
      begin
         case Var.Kind is
            when Map_Kind =>
               Result.Indexed_Variables.Insert
                 (Var.Name_Node.Text, new W_Reference_Type'
                    (Value => new W_Map_Type, others => <>));

            when Text_Kind =>
               --  Text is currently modelled as a reference to a text
               --  container.
               Result.Indexed_Variables.Insert
                 (Var.Name_Node.Text, new W_Reference_Type'
                    (Value => new W_Text_Vector_Type, others => <>));

            when others =>
               Error ("global variable type not yet supported");

         end case;
      end Allocate_Variable;

   begin
      if Object_For_Entity_Registry.Contains (Name) then
         return Object_For_Entity_Registry.Element (Name);
      else
         Result := new W_Template_Instance_Type;
         Result.Defining_Entity := T_Entity (An_Entity);

         if An_Entity.all in T_Module_Type'Class then
            for V of T_Module (An_Entity).Variables_Ordered loop
               Allocate_Variable (V);
            end loop;
         elsif An_Entity.all in T_Template_Type'Class then
            --  Templates are associated with a special vector of object, the
            --  registry, that contains all instances of the type.

            Result.Indexed_Variables.Insert
              ("_registry",
               new W_Reference_Type'(Value => new W_Vector_Type, others => <>));
         else
            Error ("static entity not associated with a node");
         end if;

         --  Static entities templates don't contain sections to be evaluated,
         --  and already have their symbols initialized.
         Result.Is_Evaluated := True;

         Object_For_Entity_Registry.Insert (Name, W_Object (Result));
         return W_Object (Result);
      end if;
   end Get_Object_For_Entity;

   function Make_Parameter
     (Name : Text_Type; Is_Optional : Boolean) return Parameter
   is
   begin
      return Parameter'
        (Name        => To_Unbounded_Text (Name),
         Is_Optional => Is_Optional);
   end Make_Parameter;

   function Process_Parameters
     (Profile : Parameter_Profile; Arg : T_Arg_Vectors.Vector) return Actuals_Type
   is
      Result : Actuals_Type (Profile'Range) := (others => null);

      Parameter_Index : Integer;
      In_Named_Section : Boolean := False;
      Formal : Parameter;
      Param : T_Arg;
   begin
      Parameter_Index := 1;

      for Actual_Index in 1 .. Arg.Length loop
         Param := Arg.Element (Integer (Actual_Index));

         if Param.Name /= "" then
            In_Named_Section := True;

            declare
               Name : Text_Type := To_Text (Param.Name);
               Found : Boolean := False;
            begin
               for I in Profile'Range loop
                  if Profile (I).Name = Name then
                     Parameter_Index := I;
                     Formal := Profile (Parameter_Index);
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Error ("parameter name '"  & Name & "' doesn't exit");
               end if;
            end;
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;

            Formal := Profile (Parameter_Index);
         end if;

         Result (Parameter_Index) := Param.Expr;

         Parameter_Index := Parameter_Index + 1;
      end loop;

      return Result;
   end Process_Parameters;

   procedure Push_Match_Result
     (Object              : W_Object;
      Matching_Expression : T_Expr)
   is
   begin
      if Matching_Expression = null then
         Push_Object (Object);
      else
         Push_Frame_Context;
         Top_Frame.Top_Context.Outer_Object := Object;
         Top_Frame.Top_Context.Match_Mode := Match_Ref_Default;

         Evaluate_Expression (Matching_Expression);

         if Pop_Object /= Match_False then
            Push_Object (Object);
         else
            Push_Match_False;
         end if;
      end if;
   end Push_Match_Result;

   procedure Push_Match_Self_Result
     (Self                : W_Object;
      Matching_Expression : T_Expr) is
   begin
      if Matching_Expression = null then
         Push_Object (Self);
      else
         Push_Frame_Context;
         Top_Frame.Top_Context.Outer_Object := Self;
         Top_Frame.Top_Context.Match_Mode := Match_Ref_Default;

         Push_Implicit_Self (Self);
         Evaluate_Expression (Matching_Expression);

         if Pop_Object /= Match_False then
            Pop_Object; -- Pop self
            Push_Object (Self);
         else
            Pop_Object; -- Pop self
            Push_Match_False;
         end if;

         Pop_Frame_Context;
      end if;
   end Push_Match_Self_Result;

   procedure Handle_Call_Parameters
     (Args : T_Arg_Vectors.Vector;
      Evaluate_Parameter : access procedure
        (Name : Text_Type; Position : Integer; Value : T_Expr))
   is
      Parameter_Index : Integer;
      In_Named_Section : Boolean := False;
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Is_Root_Selection := True;
      Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;

      Parameter_Index := 1;

      for Param of Args loop
         Push_Error_Location (Param.Name_Node);

         if Param.Name /= "" then
            In_Named_Section := True;
            Evaluate_Parameter (Param.Name_Node.Text, Parameter_Index, Param.Expr);
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;

            Evaluate_Parameter ("", Parameter_Index, Param.Expr);
         end if;

         Pop_Error_Location;
         Parameter_Index := Parameter_Index + 1;
      end loop;

      Pop_Frame_Context;
   end Handle_Call_Parameters;


end Wrapping.Runtime.Structure;
