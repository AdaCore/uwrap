------------------------------------------------------------------------------
--                                                                          --
--                                  UWrap                                   --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- UWrap is free software;  you can  redistribute it  and/or modify it      --
-- under terms of the  GNU General Public License as published  by the Free --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version.  UWrap is distributed in the hope that it will be useful, but   --
-- WITHOUT ANY WARRANTY;  without even the  implied warranty of MERCHANTA-  --
-- BILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public  --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General Public License  distributed with UWrap; see file COPYING3.  If   --
-- not, go to http://www.gnu.org/licenses for a complete copy of the        --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Wrapping.Runtime.Commands;    use Wrapping.Runtime.Commands;
with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Matching;    use Wrapping.Runtime.Matching;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;

package body Wrapping.Runtime.Nodes is

   function Is_Wrapping (Node : access W_Node_Type'Class) return Boolean is
     (Node.all in W_Template_Instance_Type'Class
      and then W_Template_Instance (Node).Origin /= null);

   procedure Call_Tmp
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   generic
      A_Mode : Traverse_Mode;
   procedure Call_Gen_Browse
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector);

   ---------------------
   -- Call_Gen_Browse --
   ---------------------

   procedure Call_Gen_Browse
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is

      procedure Generator (Expr : T_Expr);

      ---------------
      -- Generator --
      ---------------

      procedure Generator (Expr : T_Expr) is
      begin
         Push_Traverse_Result (Top_Object, A_Mode, Expr);
      end Generator;

   begin
      if Params.Length = 0 then
         Evaluate_Generator_Regexp
           (Object, null, Generator'Unrestricted_Access);
      elsif Params.Length = 1 then
         Evaluate_Generator_Regexp
           (Object, Params.Element (1).Expr, Generator'Unrestricted_Access);
      elsif Params.Length > 1 then
         Error ("matcher takes only 1 argument");
      end if;
   end Call_Gen_Browse;

   procedure Call_Browse_Parent is new Call_Gen_Browse (Parent);

   procedure Call_Browse_Child is new Call_Gen_Browse (Child_Breadth);

   procedure Call_Browse_Next is new Call_Gen_Browse (Next);

   procedure Call_Browse_Prev is new Call_Gen_Browse (Prev);

   procedure Call_Browse_Sibling is new Call_Gen_Browse (Sibling);

   procedure Call_Browse_Wrapper is new Call_Gen_Browse
     (Wrapping.Runtime.Structure.Wrapper);

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child (Parent, Child : access W_Node_Type'Class) is
   begin
      Child.Parent := W_Node (Parent);

      if Parent.Children_Ordered.Length > 0 then
         Parent.Children_Ordered.Last_Element.Next := W_Node (Child);
         Child.Prev := Parent.Children_Ordered.Last_Element;
      end if;

      Parent.Children_Ordered.Append (W_Node (Child));
   end Add_Child;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (Parent, Child : access W_Node_Type'Class; Name : Text_Type)
   is
   begin
      Add_Child (Parent, Child);
      Parent.Children_Indexed.Insert (Name, W_Node (Child));
   end Add_Child;

   --------------
   -- Add_Next --
   --------------

   procedure Add_Next (Cur, Next : access W_Node_Type'Class) is
      Found : Boolean := False with Ghost;
   begin
      Next.Next := Cur.Next;
      Next.Prev := W_Node (Cur);
      Cur.Next  := W_Node (Next);

      if Cur.Parent /= null then
         for I in
           Cur.Children_Ordered.First_Index .. Cur.Children_Ordered.Last_Index
         loop
            if Cur.Children_Ordered.Element (I) = Cur then
               Cur.Children_Ordered.Insert (I + 1, W_Node (Next));
               Found := True;
               exit;
            end if;
         end loop;

         pragma Assert (Found);
      end if;
   end Add_Next;

   -----------------------------
   -- Add_Child_With_Wrapping --
   -----------------------------

   procedure Add_Child_With_Wrapping
     (Parent, Child : access W_Node_Type'Class)
   is
      Wrapped : W_Node;
   begin
      if Is_Wrapping (Parent) then
         --  Template instances that are part of the wrapping tree are never
         --  added directly. Instead, they are wrapping a hollow node created
         --  on the origin tree.

         Wrapped := new W_Hollow_Node_Type;
         Add_Child_With_Wrapping
           (W_Template_Instance (Parent).Origin, Wrapped);
         Wrapped.Wrappers_Ordered.Append (W_Template_Instance (Child));
         W_Template_Instance (Child).Origin := Wrapped;
      else
         Add_Child (Parent, Child);
      end if;
   end Add_Child_With_Wrapping;

   ------------------------------
   -- Create_Template_Instance --
   ------------------------------

   function Create_Template_Instance
     (A_Template : T_Template;
      Wrapping   : access W_Node_Type'Class;
      Register   : Boolean) return W_Template_Instance
   is
      New_Template   : W_Template_Instance;
      Template_Class : W_Template_Instance;

      Current_Template : T_Template;
   begin
      New_Template                 := new W_Template_Instance_Type;
      New_Template.Defining_Entity := T_Entity (A_Template);

      if Wrapping /= null then
         New_Template.Origin := W_Node (Wrapping);

         if Register then
            Wrapping.Wrappers_By_Name.Insert
              (A_Template.Name_Node.Text, New_Template);
            Wrapping.Wrappers_By_Full_Id.Insert
              (A_Template.Full_Name, New_Template);
            Wrapping.Wrappers_Ordered.Append (New_Template);
         end if;
      end if;

      Current_Template := A_Template;

      while Current_Template /= null loop
         Template_Class :=
           W_Template_Instance (Get_Object_For_Entity (A_Template));

         if Register then
            W_Vector
              (W_Reference
                 (Template_Class.Indexed_Variables.Element ("_registry"))
               .Value)
              .A_Vector
                .Append
                  (W_Object (New_Template));
         end if;

         Current_Template := Current_Template.Extends;
      end loop;

      if Register then
         Register_Template_Instance (New_Template);
      end if;

      return New_Template;
   end Create_Template_Instance;

   ---------------------------
   -- Get_Template_Instance --
   ---------------------------

   function Get_Wrapper
     (An_Entity : access W_Node_Type'Class; Name : Text_Type)
      return W_Template_Instance
   is
   begin
      if An_Entity.Wrappers_By_Name.Contains (Name) then
         return An_Entity.Wrappers_By_Name.Element (Name);
      else
         return null;
      end if;
   end Get_Wrapper;

   ---------------------------
   -- Get_Template_Instance --
   ---------------------------

   function Get_Wrapper
     (An_Entity : access W_Node_Type'Class; A_Template : T_Template)
      return W_Template_Instance
   is
   begin
      --  TODO: These calls to full name may be very costly, it'd be better to
      --  cache the full name in the object
      if An_Entity.Wrappers_By_Full_Id.Contains (A_Template.Full_Name) then
         return An_Entity.Wrappers_By_Full_Id.Element (A_Template.Full_Name);
      else
         return null;
      end if;
   end Get_Wrapper;

   ----------------
   -- Push_Value --
   ----------------

   overriding function Push_Value
     (An_Entity : access W_Node_Type; Name : Text_Type) return Boolean
   is
      A_Call       : Call_Access := null;
      Is_Generator : Boolean     := False;
   begin
      if Name = "parent" then
         A_Call       := Call_Browse_Parent'Access;
         Is_Generator := True;
      elsif Name = "child" then
         A_Call       := Call_Browse_Child'Access;
         Is_Generator := True;
      elsif Name = "next" then
         A_Call       := Call_Browse_Next'Access;
         Is_Generator := True;
      elsif Name = "prev" then
         A_Call       := Call_Browse_Prev'Access;
         Is_Generator := True;
      elsif Name = "sibling" then
         A_Call       := Call_Browse_Sibling'Access;
         Is_Generator := True;
      elsif Name = "wrapper" then
         A_Call       := Call_Browse_Wrapper'Access;
         Is_Generator := True;
      elsif Name = "tmp" then
         A_Call := Call_Tmp'Access;
      end if;

      if A_Call /= null then
         Push_Object
           (W_Object'
              (new W_Intrinsic_Function_Type'
                   (Prefix    => W_Object (An_Entity), Call => A_Call,
                    Generator => Is_Generator)));

         return True;
      end if;

      if Name = "language" then
         Push_Object (To_W_String (W_Node (An_Entity).Language));

         return True;
      end if;

      return False;
   end Push_Value;

   ----------------------
   -- Push_Call_Result --
   ----------------------

   overriding procedure Push_Call_Result
     (An_Entity : access W_Node_Type; Params : T_Arg_Vectors.Vector)
   is
   begin
      if Params.Length = 0 then
         Push_Match_True (An_Entity);
      elsif Params.Length = 1 then
         Push_Implicit_It (W_Object (An_Entity));
         Push_Match_Result (Params.Element (1).Expr, W_Object (An_Entity));
         Pop_Underneath_Top;
      else
         Error ("comparing with a node requires one parameter");
      end if;
   end Push_Call_Result;

   ---------------------------
   -- Match_With_Top_Object --
   ---------------------------

   function Match_With_Top_Object
     (An_Entity : access W_Node_Type) return Boolean
   is
      Other_Entity : constant W_Object := Top_Object.Dereference;
   begin
      --  By default, nodes only consider ref as being "is" matches, and calls
      --  as being "has" matches. So pass through calls before looking.

      --  TODO: Review this, maybe this call is general to all objects and not
      --  only nodes
      if Top_Context.Match_Mode = Match_Call_Default then
         return True;
      end if;

      --  If the two entities are equal, match true

      if An_Entity = Other_Entity then
         return True;
      end if;

      --  Otherwise, see if the main checker finds something.

      if W_Object_Type (An_Entity.all).Match_With_Top_Object then
         return True;
      end if;

      return False;
   end Match_With_Top_Object;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (An_Entity    : access W_Node_Type;
      A_Mode       : Traverse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
      return Visit_Action)
      return Visit_Action
   is
      function Traverse_Wrapper
        (Entity : access W_Object_Type'Class; A_Mode : Traverse_Mode)
         return Visit_Action;
      --  Wraps the default traverse function, capturing the result if not null
      --  or false.

      function Visit_Wrapper
        (Entity : access W_Object_Type'Class) return Visit_Action;
      --  Wraps the default visit function, capturing the result if not null or
      --  false.

      Current               : W_Node;
      Current_Children_List : W_Node_Vectors.Vector;
      Next_Children_List    : W_Node_Vectors.Vector;

      ----------------------
      -- Traverse_Wrapper --
      ----------------------

      function Traverse_Wrapper
        (Entity : access W_Object_Type'Class; A_Mode : Traverse_Mode)
         return Visit_Action
      is
         Temp_Result : W_Object;
         R           : Visit_Action;
      begin
         R := Entity.Traverse (A_Mode, False, Temp_Result, Visitor);

         if Temp_Result /= Match_False and then Temp_Result /= null then
            Final_Result := Temp_Result;
         end if;

         return R;
      end Traverse_Wrapper;

      -------------------
      -- Visit_Wrapper --
      -------------------

      function Visit_Wrapper
        (Entity : access W_Object_Type'Class) return Visit_Action
      is
         Temp_Result : W_Object;
         R           : Visit_Action;
      begin
         R := Visitor (Entity, Temp_Result);

         if Temp_Result /= Match_False and then Temp_Result /= null then
            Final_Result := Temp_Result;
         end if;

         return R;
      end Visit_Wrapper;

      Decision : Visit_Action := Unknown;

   begin
      Final_Result := Match_False;

      W_Node_Type'Class (An_Entity.all).Pre_Visit;

      if Include_Self then
         W_Node_Type'Class (An_Entity.all).Pre_Visit;

         case Visit_Wrapper (An_Entity) is
            when Stop =>
               return Stop;

            when Over =>
               return Over;

            when Into | Into_Override_Anchor =>
               null;

            when Unknown =>
               null;
         end case;
      end if;

      if A_Mode = Sibling then
         --  TODO: Rewrite sibling as a next starting from the first element.
         --  the current version doesn't handle properly regular expressions
         case Traverse_Wrapper (An_Entity, Prev) is
            when Stop =>
               return Stop;

            when Over =>
               return Over;

            when Into | Into_Override_Anchor =>
               null;

            when Unknown =>
               null;
         end case;

         return Traverse_Wrapper (An_Entity, Next);
      elsif A_Mode = Wrapper then
         for T of An_Entity.Wrappers_Ordered loop
            W_Node_Type'Class (T.all).Pre_Visit;

            case Visit_Wrapper (T) is
               when Over =>
                  null;

               when Stop =>
                  return Stop;

               when Into | Into_Override_Anchor =>
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

         when Sibling | Wrapper =>
            null;

      end case;

      if A_Mode = Child_Breadth then
         loop
            for C of Current_Children_List loop
               C.Pre_Visit;

               Decision := Visit_Wrapper (C);

               case Decision is
                  when Stop =>
                     return Stop;

                  when Over =>
                     null;

                  when Into | Into_Override_Anchor =>
                     if Decision = Into_Override_Anchor
                       or else not Top_Context.Regexpr_Anchored
                     then
                        for C2 of C.Children_Ordered loop
                           Next_Children_List.Append (C2);
                        end loop;
                     end if;

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
            Current.Pre_Visit;

            Decision := Visit_Wrapper (Current);

            case Decision is
               when Stop =>
                  return Stop;

               when Over =>
                  null;

               when Into | Into_Override_Anchor =>
                  if A_Mode = Child_Depth then
                     Decision := Traverse_Wrapper (Current, A_Mode);

                     case Decision is
                        when Stop =>
                           return Stop;

                        when others =>
                           null;
                     end case;
                  end if;

               when Unknown =>
                  null;
            end case;

            if Top_Context.Regexpr_Anchored
              and then Decision /= Into_Override_Anchor
            then
               return Stop;
            end if;

            case A_Mode is
               when Parent =>
                  Current := Current.Parent;

               when Prev =>
                  Current := Current.Prev;

               when Next | Child_Depth =>
                  Current := Current.Next;

               when Child_Breadth =>
                  null;

               when Sibling | Wrapper =>
                  null;

            end case;
         end loop;
      end if;

      return Into;
   end Traverse;

   --------------------------
   -- Push_Traverse_Result --
   --------------------------

   procedure Push_Traverse_Result
     (An_Entity        : access W_Node_Type;
      A_Mode           : Traverse_Mode;
      Match_Expression : T_Expr)
   is

      function Visitor
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action;

      pragma Warnings (Off);
      --  For later use. Pragma Unreferenced doesn't work here as the procedure
      --  is referened by itself.
      function Create_Hollow_Next
        (Prev : access W_Node_Type'Class) return W_Hollow_Node;
      pragma Warnings (On);

      procedure Allocate (E : access W_Object_Type'Class);

      Visited : Boolean := False;

      -------------
      -- Visitor --
      -------------

      function Visitor
        (E : access W_Object_Type'Class;
         Result : out W_Object)
         return Visit_Action
      is
         Action : Visit_Action;
      begin
         Visited := True;
         Action := Process_Generated_Value (E, Match_Expression);
         Result := Pop_Object;

         return Action;
      end Visitor;

      ------------------------
      -- Create_Hollow_Next --
      ------------------------

      function Create_Hollow_Next
        (Prev : access W_Node_Type'Class) return W_Hollow_Node
      is
         Wrapped  : W_Hollow_Node;
         New_Node : constant W_Hollow_Node := new W_Hollow_Node_Type;
      begin
         if Is_Wrapping (Prev) then
            Wrapped := Create_Hollow_Next (W_Template_Instance (Prev).Origin);
            Wrapped.Wrappers_Ordered.Append (W_Template_Instance (New_Node));
            New_Node.Origin := W_Node (Wrapped);
         else
            --  TODO: What if parent is null?
            Add_Child (Prev.Parent, New_Node);
         end if;

         return New_Node;
      end Create_Hollow_Next;

      --------------
      -- Allocate --
      --------------

      procedure Allocate (E : access W_Object_Type'Class) is
      begin
         --  TODO: We need to be able to cancel allocation if the entire
         --  research happens to be false

         case A_Mode is
            when Child_Depth | Child_Breadth =>
               Add_Child_With_Wrapping (An_Entity, W_Node_Type (E.all)'Access);

            when others =>
               Error ("allocation not implemented on the enclosing function");
         end case;
      end Allocate;

      Found  : Boolean;
      Result : W_Object;
   begin
      Push_Frame_Context;

      --  When doing a traversal, e.g. in:
      --     child (x or new (y ())
      --  the expression "x or new (y ()) must first be evaluated against all
      --  the nodes in the child to check if one match. Only if none match will
      --  we look for one node that validates the expression while allowing
      --  the new.

      Top_Context.Allow_Allocate := False;

      Found :=
        W_Node_Type'Class (An_Entity.all).Traverse
        (A_Mode, False, Result, Visitor'Access) =
        Stop;

      if not Found and then Match_Expression /= null
        and then Match_Expression.Has_New
      then
         --  We could not find a matching node in the traversal. Now allow
         --  allocate if there's a new and check if there's an expression
         --  that matches with it.

         Top_Context.Allow_Allocate := True;
         Top_Context.Allocate_Callback := Allocate'Unrestricted_Access;
         Visited := False;

         Found :=
           W_Node_Type'Class (An_Entity.all).Traverse
           (A_Mode, False, Result, Visitor'Access) =
           Stop;

         if not Visited then
            --  If still not found, there is still a possibilty that this
            --  can match without any object valid, and then create the
            --  first element. For example, we could be in a sitation like:
            --     child (new (something ())
            --  In which case if there's no child, we nonetheless want to
            --  create the child. We however want to prevent situations like:
            --     child (x and new (something ())
            --  Where if there's no child at all, x will never match because
            --  there no element matching x in the first place, which is the
            --  reason why we're visiting a dummy entity that will presumably
            --  not match anything.

            Found := Visitor (Match_False, Result) = Stop;
         end if;
      end if;

      Pop_Frame_Context;
      Push_Frame_Context;
      Top_Context.Match_Mode := Match_Ref_Default;

      if not Found
        and then not
          (Top_Context.Match_Mode /= Match_None
           or else Top_Context.Yield_Callback /= null)
      then
         Error ("no result found for browsing function");
      end if;

      Pop_Frame_Context;

      if Result /= null then
         Push_Object (Result);
      else
         Push_Match_False;
      end if;
   end Push_Traverse_Result;

   ----------------
   -- Push_Value --
   ----------------

   function Push_Value
     (An_Entity : access W_Template_Instance_Type; Name : Text_Type)
      return Boolean
   is
   begin
      if W_Node_Type (An_Entity.all).Push_Value (Name) then
         return True;
      end if;

      --  First cover the case of a variable or a pattern

      if Name = "origin" then
         if An_Entity.Origin /= null then
            Push_Object (An_Entity.Origin);

            return True;
         end if;
      elsif Name = "kind" then
         Push_Object (To_W_String (An_Entity.Defining_Entity.Full_Name));

         return True;
      elsif An_Entity.Indexed_Variables.Contains (Name) then
         Push_Object (An_Entity.Indexed_Variables.Element (Name));
         return True;
      end if;

      return False;
   end Push_Value;

   ---------------------------
   -- Match_With_Top_Object --
   ---------------------------

   overriding function Match_With_Top_Object
     (An_Entity : access W_Template_Instance_Type) return Boolean
   is
      Other_Entity : constant W_Object := Top_Object.Dereference;
   begin
      --  Special treatment for static entities, that are always checked in
      --  "is" mode

      if Other_Entity.all in W_Static_Entity_Type'Class then
         if Top_Context.Match_Mode in
           Match_Call_Default | Match_Ref_Default | Match_Is
         then
            if not Instance_Of
              (T_Template (An_Entity.Defining_Entity),
               T_Template (W_Static_Entity (Other_Entity).An_Entity))
            then
               Pop_Object;
               Push_Match_False;
            end if;
         else
            Error ("match has not available for static entity");
         end if;

         return True;
      end if;

      if W_Node_Type (An_Entity.all).Match_With_Top_Object then
         return True;
      end if;

      return False;
   end Match_With_Top_Object;

   --------------
   -- Traverse --
   --------------

   overriding function Traverse
     (An_Entity  : access W_Template_Instance_Type; A_Mode : Traverse_Mode;
      Include_Self : Boolean; Final_Result : out W_Object;
      Visitor    : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
      return Visit_Action)
      return Visit_Action
   is

      function Template_Visitor
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action;

      ----------------------
      -- Template_Visitor --
      ----------------------

      function Template_Visitor
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action
      is
         Current_Result : W_Object;
         Last_Decision  : Visit_Action := Into;
      begin
         Result := Match_False;

         if E.all in W_Node_Type'Class then
            if W_Node (E).Wrappers_Ordered.Length = 0 then
               --  When there's no template for a given node, we consider this
               --  note to be non-existent from the template browsing point
               --  of view. As a result, anchored browsing should be allowed
               --  to look at the next level of nodes as if it was directly
               --  adjacent.

               return Into_Override_Anchor;
            else
               Push_Frame_Context;
               Top_Context.Is_First_Matching_Wrapper := True;

               for T of W_Node (E).Wrappers_Ordered loop
                  Last_Decision := Visitor (T, Current_Result);

                  if Current_Result /= Match_False
                    and then Current_Result /= null
                  then
                     Top_Context.Is_First_Matching_Wrapper := False;
                     Result := Current_Result;
                  end if;

                  exit when Last_Decision = Stop;
               end loop;

               Pop_Frame_Context;
            end if;
         end if;

         return Last_Decision;
      end Template_Visitor;

      Last_Decision : Visit_Action := Into;
      Result        : W_Object;
   begin
      Result := Match_False;

      --  A template instance either belong to an input tree (if has been
      --  created through a new from an input node) or a wrapping tree (in
      --  all other cases). If it doesn't have an origin set, it's part of the
      --  input tree, in this case fallback to the normal traversal. Otherwise,
      --  use specialized traversing using the original tree as the backbone of
      --  the iteration.
      if An_Entity.Origin = null then
         Last_Decision :=
           W_Node_Type (An_Entity.all).Traverse
           (A_Mode, Include_Self, Result, Visitor);
      else
         Last_Decision :=
           An_Entity.Origin.Traverse
             (A_Mode, False, Result, Template_Visitor'Access);
      end if;

      if Result /= null and then Result /= Match_False then
         Final_Result := Result;
      end if;

      return Last_Decision;
   end Traverse;

   --------------
   -- Call_Tmp --
   --------------

   procedure Call_Tmp
     (Object : access W_Object_Type'Class; Params : T_Arg_Vectors.Vector)
   is
      Slice : Buffer_Slice;
   begin
      if Params.Length = 0 then
         Push_Temporary_Name ("", W_Node (Object).Tmp_Counter);
      elsif Params.Length = 1 then
         Evaluate_Expression (Params.Element (1).Expr);
         Push_Buffer_Cursor;

         Slice := Pop_Object.Write_String;
         Push_Temporary_Name
           (Buffer.Str (Slice.First.Offset .. Slice.Last.Offset),
            W_Node (Object).Tmp_Counter);
         Pop_Buffer_Cursor;
      else
         Error ("tmp only accepts one argument");
      end if;
   end Call_Tmp;

end Wrapping.Runtime.Nodes;
