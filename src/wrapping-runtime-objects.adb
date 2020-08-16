with Ada.Containers; use Ada.Containers;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with Wrapping.Runtime.Analysis; use Wrapping.Runtime.Analysis;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Libtemplatelang.Common; use Libtemplatelang.Common;

package body Wrapping.Runtime.Objects is

   function Is_Wrapping (Node : access W_Node_Type'Class) return Boolean is
     (Node.all in W_Template_Instance_Type'Class
      and then W_Template_Instance (Node).Origin /= null);

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
      Params : T_Arg_Vectors.Vector);

   procedure Call_Gen_Browse
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
   begin
      if Params.Length = 0 then
         W_Node_Type'Class (Object.all).Evaluate_Generator_Regexp
           (A_Mode, null);
      elsif Params.Length = 1 then
         W_Node_Type'Class (Object.all).Evaluate_Generator_Regexp
           (A_Mode, Params.Element (1).Expr);
      elsif Params.Length > 1 then
         Error ("matcher takes only 1 argument");
      end if;
   end Call_Gen_Browse;

   procedure Call_Browse_Parent is new Call_Gen_Browse (Parent);
   procedure Call_Browse_Child is new Call_Gen_Browse (Child_Breadth);
   procedure Call_Browse_Next is new Call_Gen_Browse (Next);
   procedure Call_Browse_Prev is new Call_Gen_Browse (Prev);
   procedure Call_Browse_Sibling is new Call_Gen_Browse (Sibling);
   procedure Call_Browse_Wrapper is new Call_Gen_Browse (Wrapping.Runtime.Structure.Wrapper);

   procedure Call_Browse_Self
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
   begin
      --  TODO: This is probably never executed, as self is an object directly
      --  returned by push_value on nodes.
      if Params.Length = 0 then
         Push_Match_True (Object);
      elsif Params.Length = 1 then
         Push_Match_Self_Result
           (W_Object (Object),
            Params.Element (1).Expr);
      else
         Error ("self only takes 1 argument");
      end if;
   end Call_Browse_Self;

   procedure Call_Tmp
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
   begin
      if Params.Length = 0 then
         Push_Temporary_Name
           ("",
            W_Node (Object).Tmp_Counter);
      elsif Params.Length = 1 then
         Push_Frame_Context;
         Top_Frame.Top_Context.Match_Mode := Match_None;
         Evaluate_Expression (Params.Element (1).Expr);
         Pop_Frame_Context;

         Push_Temporary_Name
           (Pop_Object.To_String,
            W_Node (Object).Tmp_Counter);
      else
         Error ("tmp only accepts one argument");
      end if;
   end Call_Tmp;

   procedure Call_Insert
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
      P1, P2 : W_Object;
   begin
      if Object.all in W_Map_Type then
         if Params.Length /= 2 then
            Error ("two parameters expected for insert");
         end if;

         P1 := Evaluate_Expression (Params.Element (1).Expr);
         P2 := Evaluate_Expression (Params.Element (2).Expr);

         W_Map (Object).A_Map.Insert (P1, P2);
      elsif Object.all in W_Set_Type'Class then
         if Params.Length /= 1 then
            Error ("one parameters expected for include");
         end if;

         P1 := Evaluate_Expression (Params.Element (1).Expr);

         W_Set (Object).A_Set.Insert (P1);
      end if;

      Push_Object (W_Object (Object));
   end Call_Insert;

   procedure Call_Include
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
      P1, P2 : W_Object;
   begin

      if Object.all in W_Map_Type'Class then
         if Params.Length /= 2 then
            Error ("two parameters expected for include");
         end if;

         P1 := Evaluate_Expression (Params.Element (1).Expr);
         P2 := Evaluate_Expression (Params.Element (2).Expr);

         W_Map (Object).A_Map.Include (P1, P2);
      elsif Object.all in W_Set_Type'Class then
         if Params.Length /= 1 then
            Error ("one parameters expected for include");
         end if;

         P1 := Evaluate_Expression (Params.Element (1).Expr);

         W_Set (Object).A_Set.Include (P1);
      end if;

      Push_Object (W_Object (Object));
   end Call_Include;

   procedure Call_Append
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
      Val : W_Object;
   begin
      if Object.all in W_Vector_Type then
         if Params.Length /= 1 then
            Error ("one parameters expected for append");
         end if;

         Val := Evaluate_Expression (Params.Element (1).Expr);

         W_Vector (Object).A_Vector.Append (Val);
      end if;

      Push_Object (W_Object (Object));
   end Call_Append;

   procedure Call_Get
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
      Key : W_Object;
      Index : Integer;
   begin
      if Params.Length /= 1 then
         Error ("get expects one parameter");
      end if;

      Key := Evaluate_Expression
        (Params.Element (1).Expr);

      if Object.all in W_Map_Type'Class then
         if W_Map (Object).A_Map.Contains (Key) then
            Push_Object (W_Map (Object).A_Map.Element (Key));
         elsif Top_Frame.Top_Context.Match_Mode /= Match_None then
            Push_Match_False;
         else
            Error ("map doesn't contain element " & Key.To_String);
         end if;
      elsif Object.all in W_Set_Type'Class then
         if W_Set (Object).A_Set.Contains (Key) then
            Push_Object (Key);
         elsif Top_Frame.Top_Context.Match_Mode /= Match_None then
            Push_Match_False;
         else
            Error ("set doesn't contain element " & Key.To_String);
         end if;
      elsif Object.all in W_Vector_Type'Class then
         if Key.Dereference.all not in W_Integer_Type'Class then
            Error ("expected integer index");
         end if;

         Index := W_Integer (Key).Value;

         if Index in
           W_Vector (Object).A_Vector.First_Index .. W_Vector (Object).A_Vector.Last_Index
         then
            Push_Object (W_Vector (Object).A_Vector.Element (Index));
         elsif Top_Frame.Top_Context.Match_Mode /= Match_None then
            Push_Match_False;
         else
            Error ("vector doesn't contain index " & Index'Wide_Wide_Image);
         end if;
      else
         Error ("object doesn't provide get function");
      end if;
   end Call_Get;

   procedure Call_Element
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
      Last_Action : Visit_Action;
      Result : W_Object;
      Last_Result : W_Object;
      Expr : T_Expr;

      procedure Update_Result is
      begin
         if Last_Result /= null and then Last_Result /= Match_False then
            Result := Last_Result;
         end if;
      end Update_Result;
   begin
      if Params.Length > 1 then
         Error ("element expects at most one parameter");
      end if;

      if Params.Length = 1 then
         Expr := Params.Element (1).Expr;
      end if;

      if Object.all in W_Map_Type'Class then
         declare
            Map : W_Map := W_Map (Object);
         begin
            if Map.A_Map.Length > 0 then
               for E of Map.A_Map loop
                  Last_Action := Object.Browse_Entity (E, Expr, Last_Result);
                  Update_Result;

                  exit when Last_Action = Stop;
               end loop;
            end if;
         end;
      elsif Object.all in W_Set_Type'Class then
         declare
            Set : W_Set := W_Set (Object);
         begin
            if Set.A_Set.Length > 0 then
               for E of Set.A_Set loop
                  Last_Action := Object.Browse_Entity (E, Expr, Last_Result);
                  Update_Result;

                  exit when Last_Action = Stop;
               end loop;
            end if;
         end;
      elsif Object.all in W_Vector_Type'Class then
         declare
            Vector : W_Vector := W_Vector (Object);
         begin
            if Vector.A_Vector.Length > 0 then
               for E of Vector.A_Vector loop
                  Last_Action := Object.Browse_Entity (E, Expr, Last_Result);
                  Update_Result;

                  exit when Last_Action = Stop;
               end loop;
            end if;
         end;
      end if;

      if Result /= null then
         Push_Object (Result);
      else
         Push_Match_False;
      end if;
   end Call_Element;

   procedure Call_Find
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
      A_Template : W_Template_Instance := W_Template_Instance (Object);
      Registry : W_Vector;
      Last_Action : Visit_Action;
      Last_Result : W_Object;
   begin
      if Params.Length > 1 then
         Error ("find expects at most one parameter");
      end if;

      Registry := W_Vector
        (W_Reference
           (A_Template.Indexed_Variables.Element ("_registry")).Value);

      if Registry.A_Vector.Length = 0 then
         Push_Match_False;
      elsif Params.Length = 0 then
         Push_Match_True (Registry.A_Vector.First_Element);
      else
         for T of Registry.A_Vector loop
            Last_Action := Object.Browse_Entity
              (T, Params.Element (1).Expr, Last_Result);

            exit when Last_Action = Stop;
         end loop;

         if Last_Result /= null then
            Push_Object (Last_Result);
         else
            Push_Match_False;
         end if;
      end if;
   end Call_Find;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Reference_Type;
      Params    : T_Arg_Vectors.Vector)
   is
   begin
      An_Entity.Value.Push_Call_Result (Params);
   end Push_Call_Result;

   overriding
   function Match_With_Top_Object
     (An_Entity : access W_Reference_Type) return Boolean is
   begin
      return Match_With_Top_Object (An_Entity.Value);
   end Match_With_Top_Object;

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
      Match_Expression : T_Expr)
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
      Match_Expression : T_Expr;
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
   function Push_Value
     (An_Entity : access W_Vector_Type;
      Name      : Text_Type) return Boolean
   is
       Call : Call_Access;
   begin
      if Name = "element" then
         Call := Call_Element'Access;
      elsif Name = "get" then
         Call := Call_Get'Access;
      elsif Name = "append" then
         Call := Call_Append'Access;
      end if;

      if Call /= null then
         Push_Object
           (W_Object'(new W_Intrinsic_Function_Type'
              (Prefix => W_Object (An_Entity),
               Call => Call)));
         return True;
      else
         return False;
      end if;
   end Push_Value;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Vector_Type;
      Params    : T_Arg_Vectors.Vector)
   is
      Result : W_Object;
   begin
      --  TODO: This will essentially enable checks against strings, which is
      --  useful when vector indeed represent strings. Verify if
      --  this is OK. We may need a specific vector string type for this, and
      --  have a more comprehensive test here.

      if Params.Length = 0 then
         Push_Match_True (An_Entity);
      elsif Params.Length = 1 then
         Push_Implicit_Self (An_Entity);
         Evaluate_Expression (Params.Element (1).Expr);
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
     (An_Entity : access W_Set_Type;
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
      elsif Name = "get" then
         Call := Call_Get'Access;
      end if;

      if Call /= null then
         Push_Object
           (W_Object'(new W_Intrinsic_Function_Type'
              (Prefix => W_Object (An_Entity),
               Call => Call)));
         return True;
      else
         return False;
      end if;
   end Push_Value;

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
      elsif Name = "get" then
         Call := Call_Get'Access;
      end if;

      if Call /= null then
         Push_Object
           (W_Object'(new W_Intrinsic_Function_Type'
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
      Params    : T_Arg_Vectors.Vector)
   is
      Result : W_Object;
      Action : Visit_Action;
   begin
      --  Maps are browsing objects, they behave like various other tree browsing
      --  functions.
      --  TODO: there is no support for object allocation here at this stage,
      --  which is not completely obvious (needs to implement some kind of a
      --  tuple to create key and value at the same time).

      if Params.Length /= 1 then
         Error ("expected one argument for browsing map");
      end if;

      for E of An_Entity.A_Map loop
         Action :=
           An_Entity.Browse_Entity (E, Params.Element (1).Expr, Result);

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
   function Lt
     (Left : access W_String_Type; Right : access W_Object_Type'Class)
      return Boolean
   is
   begin
      if Left = Right then
         return False;
      elsif Right.all not in W_String_Type'Class then
         return W_Text_Expression_Type (Left.all).Lt (Right);
      else
         return Left.To_String < Right.To_String;
      end if;
   end Lt;

   overriding
   function Eq
     (Left : access W_String_Type; Right : access W_Object_Type'Class)
      return Boolean
   is
   begin
      if Left = Right then
         return True;
      elsif Right.all not in W_String_Type'Class then
         return W_Text_Expression_Type (Left.all).Eq (Right);
      else
         return Left.To_String = Right.To_String;
      end if;
   end Eq;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Text_Expression_Type;
      Params    : T_Arg_Vectors.Vector)
   is
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Outer_Object := W_Object (An_Entity);

      --  TODO: Should that be the high level call result?
      if Params.Length = 0 then
         Push_Match_True (An_Entity);
      elsif Params.Length = 1 then
         Evaluate_Expression (Params.Element (1).Expr);
      else
         Error ("string comparison takes one argument");
      end if;

      Pop_Frame_Context;
   end Push_Call_Result;

   overriding
   function To_String (Object : W_Regexp_Type) return Text_Type is
   begin
      return Object.Value.To_String;
   end To_String;

   overriding
   function To_String (Object : W_Text_Conversion_Type) return Text_Type is
   begin
      return Object.An_Object.To_String;
   end To_String;

   function To_String (Object : W_Text_Vector_Type) return Text_Type is
      Result : Unbounded_Text_Type;
   begin
      for T of Object.A_Vector loop
         Append (Result, T.To_String);
      end loop;

      return To_Text (Result);
   end To_String;

   overriding
   function To_String (Object : W_Text_Reindent_Type) return Text_Type is
   begin
      return Reindent (Object.Indent, Object.Content.To_String, False);
   end To_String;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Intrinsic_Function_Type;
      Params    : T_Arg_Vectors.Vector) is
   begin
      An_Entity.Call (An_Entity.Prefix, Params);
   end Push_Call_Result;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Function_Type;
      Params    : T_Arg_Vectors.Vector)
   is
      Calling_Frame : Data_Frame;
      Called_Frame : Data_Frame;
      Temp_Symbols : W_Object_Maps.Map;

      procedure Evaluate_Parameter
        (Name : Text_Type; Position : Integer; Value : T_Expr)
      is
         Computed_Name : Text_Type :=
           (if Name = "" then
               An_Entity.A_Function.Arguments_Ordered.Element (Position).Name_Node.Text
            else
               Name);
      begin
         Temp_Symbols.Insert (Computed_Name, Evaluate_Expression (Value));
      end Evaluate_Parameter;

      Last_Picked : W_Object;

      procedure Pick_Callback (Object : W_Object)
      is
      begin
         --  When reaching a value to be picked on a function f, either:
         --  (1) the caller is not an expansion, in which case we found
         --      the value, we can interrupt the above expansion if any.
         --  (2) the parent is an expansion, e.g. f ().all(). In this case, we
         --      restore temporarily frame parent frame (the frame of the
         --      caller) and execute the rest of the caller actions for the
         --      specific value picked, then go back fetching other values for
         --      the function.

         if Calling_Frame.Top_Context.Is_Expanding_Context = False then
            Last_Picked := Object;
            Top_Frame.Interrupt_Program := True;
         else
            Top_Frame := Calling_Frame;
            Push_Implicit_Self (Object);
            Calling_Frame.Top_Context.Expand_Action.all;
            Last_Picked := Pop_Object;
            Pop_Object;
            Top_Frame := Called_Frame;
         end if;
      end Pick_Callback;

      Prev_Self : W_Object := Get_Implicit_Self;
   begin
      Handle_Call_Parameters
        (Params, Evaluate_Parameter'Access);

      Calling_Frame := Top_Frame;

      Push_Frame (An_Entity.A_Function);
      Called_Frame := Top_Frame;

      Push_Implicit_Self (Prev_Self);
      Top_Frame.Symbols.Move (Temp_Symbols);
      Top_Frame.Top_Context.Pick_Callback := Pick_Callback'Unrestricted_Access;
      Top_Frame.Top_Context.Is_Expanding_Context := False;
      Top_Frame.Top_Context.Expand_Action := null;

      Handle_Command_Sequence (An_Entity.A_Function.Program.First_Element);

      Pop_Frame;

      if Last_Picked /= null then
         Push_Object (Last_Picked);
      else
         Push_Match_False;
      end if;
   end Push_Call_Result;

   overriding
   function Push_Value
     (An_Entity : access W_Static_Entity_Type;
      Name      : Text_Type) return Boolean
   is
      A_Semantic_Entity : T_Entity;
   begin
      if An_Entity.An_Entity.all in T_Template_Type'Class then
         if Name = "find" then
            Push_Object
              (W_Object'
                 (new W_Intrinsic_Function_Type'
                      (Prefix => Get_Object_For_Entity (An_Entity.An_Entity),
                       Call => Call_Find'Access)));
            return True;
         end if;
      end if;

      if An_Entity.An_Entity.Children_Indexed.Contains (Name) then
         A_Semantic_Entity := An_Entity.An_Entity.Children_Indexed.Element (Name);

         if A_Semantic_Entity.all in T_Var_Type'Class then
            --  We found a reference to a Var. This means that we need to process
            --  the node corresponding to this module, and retreive the actual
            --  variable value.

            return Get_Object_For_Entity (An_Entity.An_Entity).Push_Value (Name);
         else
            Push_Object
              (W_Object'(new W_Static_Entity_Type'(An_Entity => A_Semantic_Entity)));

            return True;
         end if;
      end if;

      return False;
   end Push_Value;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Static_Entity_Type;
      Params    : T_Arg_Vectors.Vector)
   is
      Self_Object   : W_Object;
      Prefix        : W_Template_Instance;
      Result        : W_Object;
   begin
      --  Matching an static entity reference means two things:
      --    *  First, check that this entity reference exist in the context,
      --       that is it's of a subtype of the enclosing self.
      --    *  Second, check that the expression, if any, corresponds to the
      --       components of the enclosing self

      Self_Object := Get_Implicit_Self;

      --  This function currently only operates on template instances and
      --  template types. Check that self is a template of the right type.

      if Self_Object.all not in W_Template_Instance_Type then
         Push_Match_False;
         return;
      end if;

      Prefix := W_Template_Instance (Self_Object);

      --  If we're of the right type, then push the implicit self so that
      --  the stack starts with an implicit entity at the top, and check
      --  the result.

      if Params.Length = 0 then
         Push_Match_True (Self_Object);
         return;
      elsif Params.Length = 1 then
         Push_Implicit_Self (Self_Object);
         Evaluate_Expression (Params.Element (1).Expr);
         Result := Pop_Object;
         Pop_Object;

         if Result /= Match_False then
            --  If the result is good, then the result of this match is the
            --  matched object.

            Push_Match_True (Self_Object);
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

   procedure Add_Wrapping_Child (Parent, Child : access W_Node_Type'Class) is
      Wrapped : W_Node;
   begin
      if Is_Wrapping (Parent) then
         --  Template instances that are part of the wrapping tree
         --  are never added directly. Instead, they are wrapping a
         --  hollow node created on the origin tree.

         Wrapped := new W_Hollow_Node_Type;
         Add_Wrapping_Child (W_Template_Instance (Parent).Origin, Wrapped);
         Wrapped.Templates_Ordered.Append (W_Template_Instance (Child));
         W_Template_Instance (Child).Origin := W_Node (Wrapped);
      else
         Add_Child (Parent, Child);
      end if;
   end Add_Wrapping_Child;

   function Create_Template_Instance
     (An_Entity : access W_Node_Type'Class;
      A_Template : T_Template) return W_Template_Instance
   is
      New_Template : W_Template_Instance;
      Template_Class : W_Template_Instance;

      Current_Template : T_Template;
   begin
      New_Template := new W_Template_Instance_Type;
      New_Template.Defining_Entity := T_Entity (A_Template);

      if An_Entity /= null then
         New_Template.Origin := W_Node (An_Entity);

         An_Entity.Templates_By_Name.Insert (A_Template.Name_Node.Text, New_Template);
         An_Entity.Templates_By_Full_Id.Insert (A_Template.Full_Name, New_Template);
         An_Entity.Templates_Ordered.Append (New_Template);
      end if;

      Current_Template := A_Template;

      while Current_Template /= null loop
         Template_Class := W_Template_Instance (Get_Object_For_Entity (A_Template));

         W_Vector
           (W_Reference
              (Template_Class.Indexed_Variables.Element ("_registry")).Value).
             A_Vector.Append (W_Object (New_Template));

         Current_Template := Current_Template.Extends;
      end loop;

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
      A_Template : T_Template) return W_Template_Instance is
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
      elsif Name = "wrapper" then
         A_Call := Call_Browse_Wrapper'Access;
      elsif Name = "tmp" then
         A_Call := Call_Tmp'Access;
      elsif Name = "self" then
         A_Call := Call_Browse_Self'Access;
      end if;

      if A_Call /= null then
         Push_Object
           (W_Object'(new W_Intrinsic_Function_Type'
                (Prefix => W_Object (An_Entity),
                 Call => A_Call)));

         return True;
      end if;

      return False;
   end Push_Value;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Node_Type;
      Params    : T_Arg_Vectors.Vector)
   is
      Result : W_Object;
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Outer_Object := W_Object (An_Entity);

      --  TODO: this code is probably the generic call result code, not specific to
      --   node type.
      if Params.Length = 0 then
         Push_Match_True (An_Entity);
      elsif Params.Length = 1 then
         Push_Implicit_Self (W_Object (An_Entity));
         Result := Evaluate_Expression (Params.Element (1).Expr);
         Pop_Object;

         if Result = Match_False then
            Push_Match_False;
         else
            Push_Object (W_Object (An_Entity));
         end if;
      else
         Error ("comparing with a node requires one parameter");
      end if;

      Pop_Frame_Context;
   end Push_Call_Result;

   function Match_With_Top_Object
     (An_Entity : access W_Node_Type) return Boolean
   is
      Other_Entity : W_Object := Top_Object.Dereference;
   begin
      --  By default, nodes only consider ref as being "is" matches, and
      --  calls as being "has" matches. So pass through calls before looking.

      if Top_Frame.Top_Context.Match_Mode = Match_Call_Default then
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

      -- Wraps the default traverse function, capturing the result if not
      -- null or false.
      function Traverse_Wrapper
        (Entity : access W_Object_Type'Class; A_Mode : Browse_Mode)
         return Visit_Action
      is
         Temp_Result : W_Object;
         R : Visit_Action;
      begin
         R := Entity.Traverse (A_Mode, False, Temp_Result, Visitor);

         if Temp_Result /= Match_False and then Temp_Result /= null then
            Final_Result := Temp_Result;
         end if;

         return R;
      end Traverse_Wrapper;

      -- Wraps the default visit function, capturing the result if not
      -- null or false.
      function Visit_Wrapper
        (Entity : access W_Object_Type'Class) return Visit_Action
      is
         Temp_Result : W_Object;
         R : Visit_Action;
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
         -- TODO: Rewrite sibling as a next starting from the first element.
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
         for T of An_Entity.Templates_Ordered loop
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
               W_Node_Type'Class (C.all).Pre_Visit;

               Decision := Visit_Wrapper (C);

               case Decision is
                  when Stop =>
                     return Stop;

                  when Over =>
                     null;

                  when Into | Into_Override_Anchor =>
                     if Decision = Into_Override_Anchor
                       or else not Top_Frame.Top_Context.Regexpr_Anchored
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
            W_Node_Type'Class (Current.all).Pre_Visit;

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

            if Top_Frame.Top_Context.Regexpr_Anchored
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

   procedure Evaluate_Generator_Regexp
     (An_Entity : access W_Node_Type;
      A_Mode    : Browse_Mode;
      Expr      : T_Expr)
   is
      Quantifiers_Hit : Integer := 0;

      Is_Original_Expansion : Boolean := Top_Frame.Top_Context.Is_Expanding_Context;
      Original_Expand_Function : Expand_Action_Type := Top_Frame.Top_Context.Expand_Action;

      procedure Yield_Action
        with Post => Top_Frame.Data_Stack.Length
          = Top_Frame.Data_Stack.Length'Old + 1;

      procedure Install_Yield_Capture is
      begin
         Top_Frame.Top_Context.Is_Expanding_Context := True;
         Top_Frame.Top_Context.Expand_Action := Yield_Action'Unrestricted_Access;
      end Install_Yield_Capture;

      procedure Restore_Yield_Capture is
      begin
         Top_Frame.Top_Context.Is_Expanding_Context := Is_Original_Expansion;
         Top_Frame.Top_Context.Expand_Action := Original_Expand_Function;
      end Restore_Yield_Capture;

      procedure Process_Right_Action is
         Result : W_Object;
      begin
         if Expr.Reg_Expr_Right /= null then
            Push_Frame_Context;

            Restore_Yield_Capture;
            Evaluate_Generator_Regexp (W_Node (Top_Object.Dereference), A_Mode, Expr.Reg_Expr_Right);
            Result := Top_Object.Dereference;

            Pop_Frame_Context;

            if Result = Match_False then
               Top_Frame.Top_Context.Visit_Decision.all := Into;
            else
               if Is_Original_Expansion then
                  Top_Frame.Top_Context.Visit_Decision.all := Into;
               else
                  Top_Frame.Top_Context.Visit_Decision.all := Stop;
               end if;
            end if;
         else
            Push_Object (Top_Object);

            if Is_Original_Expansion then
               Original_Expand_Function.all;
               Delete_Object_At_Position (-2);
               Top_Frame.Top_Context.Visit_Decision.all := Into;
            else
               Top_Frame.Top_Context.Visit_Decision.all := Stop;
            end if;
         end if;
      end Process_Right_Action;

      --  TODO: we need to generalize this concept of generator / yield
      --  instead of expand.
      procedure Yield_Action is
      begin
         Push_Frame_Context;

         if Expr.Kind = Template_Reg_Expr_Anchor then
            Top_Frame.Top_Context.Is_Expanding_Context := False;
            --  If we're evaluating an anchor at this stage, this is a right
            --  anchor. There should not be any more element available.

            Evaluate_Bowse_Functions (Top_Object, A_Mode, null);

            if Pop_Object /= Match_False then
               Push_Match_False;
            else
               Push_Object (An_Entity);
            end if;
         else
            Top_Frame.Top_Context.Regexpr_Anchored := True;
            Install_Yield_Capture;

            if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier then
               Quantifiers_Hit := Quantifiers_Hit + 1;

               if Quantifiers_Hit < Expr.Reg_Expr_Left.Min then
                  --  First, no matter the quantifier, try to reach the minimum
                  --  value
                  Evaluate_Bowse_Functions
                    (Top_Object,
                     A_Mode,
                     Expr.Reg_Expr_Left.Quantifier_Expr);
               elsif Quantifiers_Hit = Expr.Reg_Expr_Left.Max then
                  -- Second, if we hit the max, move on to the right action
                  Process_Right_Action;
               else
                  --  Then if the quantifier is many, try to reach as many
                  --  as possible. If few, see if one is necessary.

                  case Expr.Reg_Expr_Left.Node.As_Reg_Expr_Quantifier.F_Quantifier.Kind is
                     when Template_Operator_Many =>
                        Evaluate_Bowse_Functions
                          (Top_Object,
                           A_Mode,
                           Expr.Reg_Expr_Left.Quantifier_Expr);

                        if Top_Object = Match_False then
                           --  If the result is false, we went one element too
                           --  far. Pop it and process the right action.

                           Pop_Object;
                           Process_Right_Action;
                        elsif Is_Original_Expansion then
                           --  If we're doing an expansion, then we need to
                           --  consider all cases of potential children

                           Process_Right_Action;
                           Delete_Object_At_Position (-2);
                        end if;

                     when Template_Operator_Few =>
                        Process_Right_Action;

                        if Top_Object = Match_False then
                           --  If the result is false, the right action didn't
                           --  work, try to process another node under the
                           --  current condition

                           Pop_Object;
                           Evaluate_Bowse_Functions
                             (Top_Object,
                              A_Mode,
                              Expr.Reg_Expr_Left.Quantifier_Expr);
                        elsif Is_Original_Expansion then
                           --  If we're doing an expansion, then we need to
                           --  consider all cases of potential children

                           Evaluate_Bowse_Functions
                             (Top_Object,
                              A_Mode,
                              Expr.Reg_Expr_Left.Quantifier_Expr);
                           Delete_Object_At_Position (-2);
                        end if;

                     when others =>
                        Error ("unexpected quantifier kind");
                  end case;
               end if;
            else
               Process_Right_Action;
            end if;
         end if;

         Pop_Frame_Context;
      end Yield_Action;

      Result : W_Object;

      Dispatching_Entity : W_Node := W_Node (An_Entity);
   begin
      if Expr = null
        or else Expr.Kind not in Template_Reg_Expr_Anchor | Template_Reg_Expr
      then
         Evaluate_Bowse_Functions
           (Dispatching_Entity, A_Mode, Expr);

         return;
      end if;

      Push_Frame_Context;

      if Expr.Kind = Template_Reg_Expr_Anchor then
         Top_Frame.Top_Context.Is_Expanding_Context := False;

         Evaluate_Bowse_Functions
           (Dispatching_Entity, A_Mode, null);

         Result := Pop_Object.Dereference;

         if Result = Match_False then
            Push_Object (An_Entity);
         else
            Push_Match_False;
         end if;

         Top_Frame.Top_Context.Visit_Decision.all := Stop;
      else
         if Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Anchor then
            Top_Frame.Top_Context.Regexpr_Anchored := True;
            Evaluate_Generator_Regexp (Dispatching_Entity, A_Mode, Expr.Reg_Expr_Right);
         elsif Expr.Reg_Expr_Left.Kind = Template_Reg_Expr_Quantifier then
            case Expr.Reg_Expr_Left.Node.As_Reg_Expr_Quantifier.F_Quantifier.Kind is
               when Template_Operator_Many =>
                  Install_Yield_Capture;
                  Evaluate_Bowse_Functions (Dispatching_Entity, A_Mode, Expr.Reg_Expr_Left.Quantifier_Expr);
                  Restore_Yield_Capture;

                  if Top_Object = Match_False then
                     --  If we didn't find a match but the minimum requested is 0,
                     --  try to evaluate the result bypassing the quantifier.

                     if Expr.Reg_Expr_Left.Min = 0 then
                        Pop_Object;
                        Evaluate_Generator_Regexp (Dispatching_Entity, A_Mode, Expr.Reg_Expr_Right);
                     end if;
                  end if;

               when Template_Operator_Few =>
                  if Expr.Reg_Expr_Left.Min = 0 then
                     --  We are on a lazy quantifier and are accepting no matches.
                     --  First see if we can avoid the quantifier altogether

                     Evaluate_Generator_Regexp (Dispatching_Entity, A_Mode, Expr.Reg_Expr_Right);

                     if Top_Object = Match_False then
                        Pop_Object;

                        Install_Yield_Capture;
                        Evaluate_Bowse_Functions (Dispatching_Entity, A_Mode, Expr.Reg_Expr_Left.Quantifier_Expr);
                        Restore_Yield_Capture;
                     end if;
                  else
                     Install_Yield_Capture;
                     Evaluate_Bowse_Functions (Dispatching_Entity, A_Mode, Expr.Reg_Expr_Left.Quantifier_Expr);
                     Restore_Yield_Capture;
                  end if;

               when others =>
                  Error ("unexpected quantifier kind");
            end case;
         else
            Install_Yield_Capture;
            Evaluate_Bowse_Functions (Dispatching_Entity, A_Mode, Expr.Reg_Expr_Left);
            Restore_Yield_Capture;
         end if;
      end if;

      Pop_Frame_Context;
   end Evaluate_Generator_Regexp;

   procedure Evaluate_Bowse_Functions
     (An_Entity         : access W_Node_Type;
      A_Mode            : Browse_Mode;
      Match_Expression  : T_Expr)
   is
      function Visitor
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action is
      begin
         return Browse_Entity (An_Entity, E, Match_Expression, Result);
      end Visitor;

      function Create_Hollow_Next (Prev : access W_Node_Type'Class) return W_Hollow_Node is
         Wrapped : W_Hollow_Node;
         New_Node : W_Hollow_Node := new W_Hollow_Node_Type;
      begin
         if Is_Wrapping (Prev) then
            Wrapped := Create_Hollow_Next (W_Template_Instance (Prev).Origin);
            Wrapped.Templates_Ordered.Append (W_Template_Instance (New_Node));
            New_Node.Origin := W_Node (Wrapped);
         else
            --  TODO: What if parent is null?
            Add_Child (Prev.Parent, New_Node);
         end if;

         return New_Node;
      end;

      procedure Allocate (E : access W_Object_Type'Class) is
      begin
         --  TODO: We need to be able to cancel allocation if the entire
         --  research happens to be false

         case A_Mode is
            when Child_Depth | Child_Breadth =>
               Add_Wrapping_Child (W_Node (An_Entity), W_Node (E));

            when others =>
               Error ("allocation not implemented on the enclosing function");
         end case;
      end Allocate;

      Found : Boolean;
      Result : W_Object;
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.An_Allocate_Callback := null;
      Top_Frame.Top_Context.Match_Mode := Match_Ref_Default;

      Found := W_Node_Type'Class(An_Entity.all).Traverse
        (A_Mode, False, Result, Visitor'Access) = Stop;

      if not Found
        and then Match_Expression /= null
        and then Match_Expression.Has_New
      then
         --  Semantic for search is to look first for matches that do not require
         --  an allocator. If none is found and if there are allocators, then
         --  re-try, this time with allocators enabled.

         if Top_Frame.Top_Context.Is_Expanding_Context then
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
      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode := Match_Ref_Default;

      if not Found and then
        not
          (Top_Frame.Top_Context.Match_Mode /= Match_None
           or else Top_Frame.Top_Context.Is_Expanding_Context)
      then
         Error ("no result found for browsing function");
      end if;

      Pop_Frame_Context;

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
      elsif Name = "kind" then
         Push_Object
           (W_Object'
              (new W_String_Type'
                   (Value => To_Unbounded_Text
                        (An_Entity.Defining_Entity.Full_Name))));

         return True;
      elsif An_Entity.Indexed_Variables.Contains (Name) then
         Push_Object (An_Entity.Indexed_Variables.Element (Name));
         return True;
      end if;

      return False;
   end Push_Value;

   overriding
   function Match_With_Top_Object
     (An_Entity : access W_Template_Instance_Type) return Boolean
   is
      Other_Entity : W_Object := Top_Object.Dereference;
   begin
      --  Special treatment for static entities, that are always checked in
      --  "is" mode

      if Other_Entity.all in W_Static_Entity_Type'Class then
         if Top_Frame.Top_Context.Match_Mode in
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
         Current_Result : W_Object;
         Last_Decision : Visit_Action := Into;
      begin
         Result := Match_False;

         if E.all in W_Node_Type'Class then
            if W_Node (E).Templates_Ordered.Length = 0 then
               --  When there's no template for a given node, we consider
               --  this note to be non-existent from the template browsing
               --  point of view. As a result, anchored browsing should be
               --  allowed to look at the next level of nodes as if it was
               --  directly adjacent.

               return Into_Override_Anchor;
            else
               for T of W_Node (E).Templates_Ordered loop
                  Last_Decision := Visitor (T, Current_Result);

                  if Current_Result /= Match_False
                    and then Current_Result /= null
                  then
                     Result := Current_Result;
                  end if;

                  if Last_Decision = Stop then
                     return Stop;
                  end if;
               end loop;
            end if;
         end if;

         return Last_Decision;
      end Template_Visitor;

      Last_Decision : Visit_Action := Into;
      Result : W_Object;
   begin
      Result := Match_False;

      --  A template instance either belong to an input tree (if has been
      --  created through a new from an input node) or a wrapping tree (in
      --  all other cases). If it doesn't have an origin set, it's part of
      --  the input tree, in this case fallback to the normal traversal.
      --  Otherwise, use specialized traversing using the original tree as the
      --  backbone of the iteration.
      if An_Entity.Origin = null then
         Last_Decision := W_Node_Type (An_Entity.all).Traverse
           (A_Mode, Include_Self, Result, Visitor);
      else
         Last_Decision := An_Entity.Origin.Traverse
           (A_Mode, False, Result, Template_Visitor'Access);
      end if;

      if Result /= null and then Result /= Match_False then
         Final_Result := Result;
      end if;

      return Last_Decision;
   end Traverse;

end Wrapping.Runtime.Objects;
