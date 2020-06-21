with Ada.Containers; use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

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

   function Get_Visible_Symbol (A_Frame: Data_Frame_Type; Name : Text_Type) return W_Object is
   begin
      if A_Frame.Symbols.Contains (Name) then
         return A_Frame.Symbols.Element (Name);
      elsif A_Frame.Parent_Frame /= null then
         return Get_Visible_Symbol (A_Frame.Parent_Frame.all, Name);
      else
         --  Return one of the global function.
         --  TODO: we probably want some mechanism to allow this to be extended
         if Name = "unindent" then
            return new W_Call_Unindent_Type;
         elsif Name = "to_lower" then
            return new W_Call_To_Lower_Type;
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

   function Browse_Entity
     (An_Entity : access W_Object_Type;
      Browsed : access W_Object_Type'Class;
      Match_Expression : Template_Node'Class;
      Result : out W_Object) return Visit_Action
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

      Expression_Result : W_Object;

   begin
      Result := null;

      --  If the match expression is null, we're only looking for the
      --  presence of a node, not its form. The result is always true.
      if Match_Expression.Is_Null then
         if Top_Frame.Top_Context.Is_Folding_Context then
            Evaluate_Fold_Function;

            return Into;
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
        and then not Top_Frame.Top_Context.Is_Folding_Context
      then
         Top_Frame.Symbols.Include
           (To_Text (Top_Frame.Top_Context.Name_Captured),
            new W_Reference_Type'
              (Value => W_Object (Browsed), others => <>));
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

            if Top_Frame.Top_Context.Is_Folding_Context then
               Error ("allocation in folding browsing functions is illegal");
            end if;

            return Stop;
         else
            if Top_Frame.Top_Context.Is_Folding_Context then
               Evaluate_Fold_Function;

               return Into;
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

   procedure Push_Call_Result
     (An_Entity : access W_Object_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) is
   begin
      Error ("non callable entity");
   end Push_Call_Result;

   Module_Registry : W_Object_Maps.Map;

   function Get_Object_For_Module
     (A_Module : Wrapping.Semantic.Structure.Module) return W_Object
   is
      Result : W_Template_Instance;
      Name : Text_Type := A_Module.Full_Name;
   begin
      if Module_Registry.Contains (Name) then
         return Module_Registry.Element (Name);
      else
         Result := new W_Template_Instance_Type;

         for V of A_Module.Variables_Ordered loop
            case V.Kind is
               when Map_Kind =>
                  Result.Symbols.Insert
                    (V.Name_Node.Text, new W_Reference_Type'
                       (Value => new W_Map_Type, others => <>));

               when Text_Kind =>
                  --  Text is currently modelled as a reference to a text
                  --  container.
                  Result.Symbols.Insert
                    (V.Name_Node.Text, new W_Reference_Type'
                       (Value => new W_Vector_Type, others => <>));

               when others =>
                  Error ("global variable type not yet supported");

            end case;
         end loop;

         Module_Registry.Insert (Name, W_Object (Result));
         return W_Object (Result);
      end if;
   end Get_Object_For_Module;

end Wrapping.Runtime.Structure;
