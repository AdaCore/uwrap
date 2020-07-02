with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings; use Ada.Strings;
with Ada.Tags; use Ada.Tags;
with GNAT.Regpat; use GNAT.Regpat;

with Langkit_Support.Diagnostics;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libtemplatelang.Common; use Libtemplatelang.Common;

with Wrapping.Regex; use Wrapping.Regex;
with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Utils; use Wrapping.Utils;
with Wrapping.Runtime.Functions; use Wrapping.Runtime.Functions;

package body Wrapping.Runtime.Analysis is

   procedure Handle_Identifier (Node : Template_Node'Class);
   procedure Handle_Call (Expr : T_Expr);
   procedure Handle_Template_Call
     (A_Template_Instance : W_Template_Instance;
      Args : T_Arg_Vectors.Vector);
   procedure Handle_Visitor_Call
     (An_Entity    : W_Node;
      A_Visitor    : T_Visitor;
      Args         : T_Arg_Vectors.Vector);
   procedure Handle_Fold (Selector : T_Expr;  Suffix : in out T_Expr_Vectors.Vector);
   procedure Handle_New (Create_Tree : T_Create_Tree);
   procedure Handle_All (Selector : T_Expr;  Suffix : T_Expr_Vectors.Vector);
   procedure Handle_Selector (Expr : T_Expr;  Suffix : in out T_Expr_Vectors.Vector);

   procedure Analyze_Replace_String
     (Expr          : T_Expr;
      On_Group      : access procedure (Index : Integer; Value : W_Object) := null;
      On_Expression : access procedure (Expr : T_Expr) := null);

   procedure Apply_Wrapping_Program
     (Self           : W_Node;
      Lexical_Scope  : access T_Entity_Type'Class;
      A_Visit_Action : in out Visit_Action);

   procedure Call_Convert_To_Text
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector)
   is
   begin
      if Params.Length = 1 then
         Push_Object
           (W_Object'
              (new W_Text_Conversion_Type'
                   (An_Object => Evaluate_Expression (Params.Element (1).Expr))));
      else
         Error ("conversion takes 1 argument");
      end if;
   end Call_Convert_To_Text;

   procedure Push_Error_Location
     (An_Entity : access T_Entity_Type'Class)
   is
   begin
      Wrapping.Push_Error_Location
        (An_Entity.Unit.Get_Filename,
         (An_Entity.Sloc.Start_Line, An_Entity.Sloc.Start_Column));
   end Push_Error_Location;

   procedure Pop_Error_Location is
   begin
      Wrapping.Pop_Error_Location;
   end Pop_Error_Location;

   procedure Push_Object (An_Object : access W_Object_Type'Class) is
   begin
      Top_Frame.Data_Stack.Append (W_Object (An_Object));
   end Push_Object;

   procedure Push_Implicit_Self (An_Entity : access W_Object_Type'Class) is
   begin
      Top_Frame.Data_Stack.Append
        (new W_Reference_Type'
           (Value => W_Object (An_Entity),
            Is_Implicit_Self => True,
            others => <>));
   end Push_Implicit_Self;

   procedure Push_Implicit_New (An_Entity : access W_Object_Type'Class) is
   begin
      Top_Frame.Data_Stack.Append
        (new W_Reference_Type'
           (Value => W_Object (An_Entity),
            Is_Implicit_New => True,
            others => <>));
   end Push_Implicit_New;

   procedure Push_Allocated_Entity (An_Entity : access W_Object_Type'Class) is
   begin
      Top_Frame.Data_Stack.Append
        (new W_Reference_Type'
           (Value => W_Object (An_Entity),
            Is_Allocated => True,
            others => <>));
   end Push_Allocated_Entity;

   procedure Push_Temporary_Name (Name : Text_Type; Counter : in out Integer) is
   begin
      if Top_Frame.Temp_Names.Contains (Name) then
         Top_Frame.Data_Stack.Append
           (new W_String_Type'
              (Value => To_Unbounded_Text (Top_Frame.Temp_Names.Element (Name))));
      else
         Counter := Counter + 1;

         declare
            Tmp : Text_Type := "Temp_" &
            (if Name /= "" then Name & "_" else "")
              & Trim (Integer'Wide_Wide_Image (Counter), Both);
         begin
            Top_Frame.Temp_Names.Insert (Name, Tmp);

            Top_Frame.Data_Stack.Append
              (new W_String_Type'(Value => To_Unbounded_Text (Tmp)));
         end;
      end if;
   end Push_Temporary_Name;

   procedure Push_Function (Prefix : W_Object; A_Call : Call_Access) is
   begin
      Push_Object
        (W_Object'
           (new W_Function_Type'
                (Prefix => Prefix,
                 Call   => A_Call)));
   end Push_Function;

   procedure Pop_Object (Number : Positive := 1) is
   begin
      Top_Frame.Data_Stack.Delete_Last (Count_Type (Number));
   end Pop_Object;

   function Pop_Object return W_Object is
      Result : W_Object;
   begin
      Result := Top_Frame.Data_Stack.Last_Element;
      Pop_Object;
      return Result;
   end Pop_Object;

   function Top_Object return W_Object is
   begin
      return Top_Frame.Data_Stack.Last_Element;
   end Top_Object;

   function Top_Is_Implicit return Boolean is
      Top : W_Object := Top_Object;
   begin
      return Top.all in W_Reference_Type'Class
        and then W_Reference (Top).Is_Implicit;
   end Top_Is_Implicit;

   procedure Push_Frame_Context is
   begin
      Push_Frame_Context (Top_Frame.Top_Context.all);
   end Push_Frame_Context;

   procedure Push_Frame_Context (Context : Frame_Context_Type) is
   begin
      Top_Frame.Top_Context := new Frame_Context_Type'
        (Parent_Context       => Top_Frame.Top_Context,
         Current_Command      => Context.Current_Command,
         Outer_Expr_Callback  => Context.Outer_Expr_Callback,
         Match_Mode           => Context.Match_Mode,
         Is_Expanding_Context => Context.Is_Expanding_Context,
         Name_Captured        => Context.Name_Captured,
         Expand_Action        => Context.Expand_Action,
         An_Allocate_Callback => Context.An_Allocate_Callback,
         Left_Value           => Context.Left_Value,
         Is_Root_Selection    => Context.Is_Root_Selection,
         Outer_Object         => Context.Outer_Object);
   end Push_Frame_Context;

   procedure Pop_Frame_Context is
   begin
      Top_Frame.Top_Context := Top_Frame.Top_Context.Parent_Context;
   end Pop_Frame_Context;

   procedure Push_Frame (Lexical_Scope : access T_Entity_Type'Class) is
      New_Frame : Data_Frame := new Data_Frame_Type;
   begin
      New_Frame.Parent_Frame := Top_Frame;
      New_Frame.Lexical_Scope := T_Entity (Lexical_Scope);
      New_Frame.Top_Context := new Frame_Context_Type;

      if Top_Frame /= null then
         --  TODO: Do we really need to carry allocate callback from frame to
         --  frame?
         New_Frame.Top_Context.An_Allocate_Callback := Top_Frame.Top_Context.An_Allocate_Callback;
      end if;

      Top_Frame := New_Frame;
   end Push_Frame;

   procedure Pop_Frame is
      procedure Free is new Ada.Unchecked_Deallocation (Data_Frame_Type, Data_Frame);

      Old_Frame : Data_Frame;
   begin
      Old_Frame := Top_Frame;
      Top_Frame := Old_Frame.Parent_Frame;

      Free (Old_Frame);
   end Pop_Frame;

   function Get_Implicit_Self return W_Object is
   begin
      for I in reverse Top_Frame.Data_Stack.First_Index .. Top_Frame.Data_Stack.Last_Index loop
         if Top_Frame.Data_Stack.Element (I).all in W_Reference_Type'Class
           and then W_Reference (Top_Frame.Data_Stack.Element (I)).Is_Implicit_Self
         then
            return W_Reference (Top_Frame.Data_Stack.Element (I)).Value;
         end if;
      end loop;

      return null;
   end Get_Implicit_Self;

   function Get_Implicit_New return W_Object is
   begin
      for I in reverse Top_Frame.Data_Stack.First_Index .. Top_Frame.Data_Stack.Last_Index loop
         if Top_Frame.Data_Stack.Element (I).all in W_Reference_Type'Class
           and then W_Reference (Top_Frame.Data_Stack.Element (I)).Is_Implicit_New
         then
            return W_Reference (Top_Frame.Data_Stack.Element (I)).Value;
         end if;
      end loop;

      return null;
   end Get_Implicit_New;

   function Match (Pattern, Text : Text_Type) return Boolean is
      Text_Str : String := To_String (Text);
      Matches : Match_Obj :=
        Match (Compile (To_String (Pattern)), Text_Str);
   begin
      if Wrapping.Regex.No_Match (Matches) then
         return False;
      end if;

      for I in 1 .. Matches.Matches.Element'Last loop
         declare
            Matched_Text : Text_Type :=
              To_Text
                (Text_Str
                   (Matches.Matches.Element (I).First .. Matches.Matches.Element (I).Last));
            Name : Text_Type := To_Text (Get_Capture_Name (Matches, I));
         begin
            Top_Frame.Matched_Groups.Append
              (new W_String_Type'
                 (Value => To_Unbounded_Text (Matched_Text)));

            if Name /= "" then
               Top_Frame.Symbols.Include
                 (Name,
                  new W_String_Type'
                    (Value => To_Unbounded_Text (Matched_Text)));
            end if;
         end;
      end loop;

      return True;
   end Match;

   procedure Apply_Template_Action
     (Self : W_Node; Template_Clause : T_Weave_Or_Wrap)
   is
      A_Template_Instance : W_Template_Instance;
      Self_Weave : Boolean := False;
      Result : W_Object;
   begin
      Push_Error_Location (Template_Clause.Node);
      Push_Implicit_Self (Self);

      if Template_Clause.Is_Null then
         --  We've set a null template - the objective is to prevent this
         --  entity to be wrapped by this template.

         if Template_Clause.Args.Length /= 1 then
            Error ("expected one argument to null");
         end if;

         Evaluate_Expression (Template_Clause.Args.Element (1).Expr);
         Result := Pop_Object.Dereference;

         if Result.all not in W_Static_Entity_Type'Class
           or else W_Static_Entity (Result).An_Entity.all
         not in T_Template_Type'Class
         then
            Error ("expected template reference");
         end if;

         Self.Forbidden_Template_Names.Include
           (T_Template (W_Static_Entity (Result).An_Entity).Full_Name);

         --  TODO: remove the template if it's already been created in the
         --  context of a weave clause
      else
         if Template_Clause.Call_Reference = null then
            --  No name to the call, that means that we're expecting to self-weave
            --  the current template.
            if Template_Clause.all not in Weave_Type'Class then
               Error ("self wrap not allowed, either weave or provide a template or visitor name");
            elsif Self.all in W_Template_Instance_Type'Class then
               A_Template_Instance := W_Template_Instance (Self);
            else
               Error ("only template instances can be self weaved");
            end if;

            Self_Weave := True;
         elsif Template_Clause.Call_Reference.all in T_Template_Type'Class then
            A_Template_Instance := Self.Get_Template_Instance
              (T_Template (Template_Clause.Call_Reference));

            if (Template_Clause.all in Weave_Type'Class
                or else A_Template_Instance = null
                or else A_Template_Instance.Is_Wrapping = False)
              and then not Self.Forbidden_Template_Names.Contains
                (Template_Clause.Call_Reference.Full_Name)
            then
               if A_Template_Instance = null then
                  A_Template_Instance := Self.Create_Template_Instance
                    (T_Template (Template_Clause.Call_Reference));
               end if;

               if Template_Clause.all in Wrap_Type'Class then
                  A_Template_Instance.Is_Wrapping := True;
               end if;
            else
               A_Template_Instance := null;
            end if;
         end if;

         if A_Template_Instance /= null then
            if not Self_Weave then
               Push_Implicit_New (A_Template_Instance);
            end if;

            Handle_Template_Call (A_Template_Instance, Template_Clause.Args);

            if not Self_Weave then
               Pop_Object;
            end if;
         end if;
      end if;

      Pop_Object;
      Pop_Error_Location;
   end Apply_Template_Action;

   procedure Handle_Command
     (Command        : T_Command;
      A_Visit_Action : in out Visit_Action)
   is
      Top : W_Object := Top_Object.Dereference;
      Self : W_Node;
   begin
      if Top = Match_False then
         return;
      elsif Top.all in W_Node_Type'Class then
         Self := W_Node (Top);
      else
         Error ("can't pick selected object");
      end if;

      if Command.Nested_Actions /= null then
         Apply_Wrapping_Program
           (Self,
            Command.Nested_Actions,
            A_Visit_Action);
      elsif Command.Template_Section /= null then
         if Command.Template_Section.A_Visit_Action /= Unknown then
            -- TODO: consider differences between weave and wrap here
            --  TODO: This doesn't consider different visits, each
            --  should have its own decision

            if not Self.Traverse_Decision_Taken then
               Self.Traverse_Decision_Taken := True;
               A_Visit_Action := Command.Template_Section.A_Visit_Action;
            end if;
         elsif Command.Template_Section.Call_Reference /= null
           or else Command.Template_Section.Args.Length /= 0
         then
            -- There is an explicit template call. Pass this on either the
            -- current template or the whole tree

            if Command.Template_Section.Call_Reference/= null
              and then Command.Template_Section.Call_Reference.all in T_Visitor_Type'Class
            then
               Handle_Visitor_Call
                 (Self,
                  T_Visitor (Command.Template_Section.Call_Reference),
                  Command.Template_Section.Args);
            else
               Apply_Template_Action (Self, Command.Template_Section);
            end if;
         else
            null;
            -- TODO: How to handle pick all () without any specific wrap? or pick self.f_something?
         end if;
      end if;
   end Handle_Command;

   procedure Apply_Wrapping_Program
     (Self : W_Node;
      Lexical_Scope     : access T_Entity_Type'Class;
      A_Visit_Action    : in out Visit_Action)
   is
      procedure Allocate (E : access W_Object_Type'Class) is
      begin
         --  when allocating an object outside of a browsing function, nothign
         --  special to do
         null;
      end Allocate;

      procedure Create_And_Set_Template_Instance
        (Command  : T_Command;
         Expression : T_Expr);
      --  This will create a template instance from the clause information,
      --  setting parameter expression and adding it to the relevant language
      --  object.

      procedure Create_And_Set_Template_Instance
        (Command : T_Command;
         Expression : T_Expr)
      is
      begin
         Evaluate_Expression (Expression);
      end Create_And_Set_Template_Instance;

      procedure Apply_Command (Command : T_Command) is
         Matched : Boolean;
         Result  : W_Object;
      begin

         --  The command is the enclosing scope for all of its clauses. It
         --  will in particular receive the matching groups and the temporary
         --  values that can be used consistently in the various clauses
         Push_Frame (Command);
         Push_Implicit_Self (Self);

         Top_Frame.Top_Context.An_Allocate_Callback := Allocate'Unrestricted_Access;
         Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;
         Top_Frame.Top_Context.Current_Command := Command;
         Top_Frame.Top_Context.Is_Root_Selection := True;
         Top_Frame.Top_Context.Outer_Object := W_Object (Self);

         if Command.Match_Expression /= null then
            Top_Frame.Top_Context.Match_Mode := Match_Ref_Default;

            Evaluate_Expression (Command.Match_Expression);

            Matched := Pop_Object /= Match_False;
            Top_Frame.Top_Context.Match_Mode := Match_None;
         else
            Matched := True;
         end if;

         if Matched then
            if Command.Pick_Expression /= null then
               --  When evaluating a pick expression, the wrapping program will
               --  be evaluated by the outer epxression callback. This caters
               --  in particular for cases where more than one object is
               --  being retreived.

               Top_Frame.Top_Context.Outer_Expr_Callback :=
                 Outer_Expression_Pick'Access;

               Result := Evaluate_Expression (Command.Pick_Expression).Dereference;

               Top_Frame.Top_Context.Outer_Expr_Callback :=
                 Outer_Expression_Match'Access;
            else
               Handle_Command (Command, A_Visit_Action);
            end if;
         else
            if Command.Else_Actions /= null then
               if Command.Else_Actions.all in T_Command_Type'Class then
                  Apply_Command (T_Command (Command.Else_Actions));
               else
                  Apply_Wrapping_Program
                    (Self, Command.Else_Actions, A_Visit_Action);
               end if;
            end if;
         end if;

         Pop_Object; -- Pop self.
         Pop_Frame;
      end Apply_Command;
   begin
      Push_Frame (Lexical_Scope);

      for Wrapping_Entity of reverse Lexical_Scope.Children_Ordered loop
         if Wrapping_Entity.all in T_Command_Type then
            Apply_Command (T_Command (Wrapping_Entity));
         elsif Wrapping_Entity.all in T_Module_Type'Class
           or else Wrapping_Entity.all in T_Namespace_Type'Class
         then
            Apply_Wrapping_Program
              (Self, Wrapping_Entity, A_Visit_Action);
         end if;
      end loop;

      Pop_Frame;
   end Apply_Wrapping_Program;

   function Analyze_Visitor
     (E : access W_Object_Type'Class;
      Result : out W_Object) return Visit_Action
   is
      A_Visit_Action : Visit_Action := Into;
      N : W_Node;
   begin
      Result := null;

      --  Check if this entity has already been analyzed through this visitor
      --  invocation.

      --  First, pop any id in the entity that may be greater than the current
      --  one. If they are greater and we're on a lower one, it means that they
      --  are over.

      if E.all not in W_Node_Type'Class then
         Error ("expected node type");
      end if;

      N := W_Node (E);

      while N.Visited_Stack.Length > 0
        and then N.Visited_Stack.Last_Element > Current_Visitor_Id
      loop
         N.Visited_Stack.Delete_Last;
      end loop;

      if N.Visited_Stack.Length > 0
        and then N.Visited_Stack.Last_Element = Current_Visitor_Id
      then
         --  If the last id in the stack is the current one, they we've already
         --  visited this entity. We already also made the decisions on sub
         --  entities. Stop the iteration.

         return Over;
      else
         --  Otherwise, stack this visitor Id and visit. We need not to remove
         --  this id at the end, as to make sure that potential iterations
         --  on this visit don't cover this node again

         N.Visited_Stack.Append (Current_Visitor_Id);

         Apply_Wrapping_Program
           (N,
            Wrapping.Semantic.Analysis.Root,
            A_Visit_Action);

         return A_Visit_Action;
      end if;
   end Analyze_Visitor;

   procedure Analyse (Root_Entity : W_Node) is
      File_Template : T_Template;
      Out_Template : T_Template;
      A_Template_Instance : W_Template_Instance;
      Dummy_Action : Visit_Action;
      Traverse_Result : W_Object;

      Files :  W_Template_Instance_Vectors.Vector;
      Output : W_Template_Instance_Vectors.Vector;
   begin
      -- Set the visitor id - we're on the main iteration, id is 0.

      Current_Visitor_Id := 0;

      Dummy_Action := Root_Entity.Traverse
        (Child_Depth, True, Traverse_Result, Analyze_Visitor'Access);

      File_Template := T_Template
        (Resolve_Module_By_Name ("standard").
             Children_Indexed.Element ("file"));

      Out_Template := T_Template
        (Resolve_Module_By_Name ("standard").
             Children_Indexed.Element ("out"));

      while Templates_To_Traverse.Length > 0 loop
         declare
            Next_Iteration : W_Template_Instance_Vectors.Vector;
         begin
            Next_Iteration.Move (Templates_To_Traverse);
            Templates_To_Traverse.Clear;

            -- Reset the visitor id - we're on the main iteration, id is 0.

            for Created_Template of Next_Iteration loop
               A_Template_Instance := W_Template_Instance (Created_Template);

               if Instance_Of
                 (T_Template (A_Template_Instance.Defining_Entity),
                  File_Template)
               then
                  Files.Append (A_Template_Instance);
               elsif Instance_Of
                 (T_Template (A_Template_Instance.Defining_Entity), Out_Template)
               then
                  Output.Append (A_Template_Instance);
               end if;
            end loop;

            Current_Visitor_Id := 0;

            for T of Next_Iteration loop
               --  The newly created wrappers need to be analyzed in order of
               --  creation. So we're not using the traverse function anymore,
               --  but instead just go through the list.

               Dummy_Action := Analyze_Visitor (T, Traverse_Result);
            end loop;
         end;
      end loop;

      for T of Output loop
         declare
            Content_Object : W_Object;
         begin
            Push_Frame (Wrapping.Semantic.Analysis.Root);

            if not T.Push_Value ("content") then
               Error ("'content' component not found in file template");
            end if;

            Content_Object := Pop_Object;

            Put (Content_Object.To_String);

            Pop_Frame;
         end;
      end loop;

      for T of Files loop
         declare
            Path_Object : W_Object;
            Content_Object : W_Object;
            Output_File : File_Type;
         begin
            Push_Frame (Wrapping.Semantic.Analysis.Root);

            if not T.Push_Value ("path") then
               Error ("'path' component not found in file template");
            end if;

            Path_Object := Pop_Object;

            if not T.Push_Value ("content") then
               Error ("'content' component not found in file template");
            end if;

            Content_Object := Pop_Object;

            Create
              (Output_File,
               Out_File,
               To_String (Path_Object.To_String));

            Put (Output_File, Content_Object.To_String);

            Close (Output_File);

            Pop_Frame;
         end;
      end loop;

   end Analyse;

   function Evaluate_Expression (Expr : T_Expr) return W_Object is
   begin
      Evaluate_Expression (Expr);

      return Pop_Object;
   end Evaluate_Expression;

   procedure Evaluate_Expression (Expr : T_Expr)
   is
      --  Some expression need to run the outer object callback. For example:
      --    match a
      --  a needs to match with self. Other do not. For example in:
      --     match has'a
      --  we need to not match (has'a) expression. Similarly, in :
      --     match a or b
      --  we need to match individually a and b, but not the expression (a or b),
      --  otherwise it wouldn't be possible to write something like:
      --     match has'a or has'b
      --  (has would discusonnect the outer match, but the overall expression
      --  would match again).
      Run_Outer_Callback : Boolean := True;

      Do_Pop_Frame_Context : Boolean := False;
   begin
      Push_Error_Location (Expr.Node);

      case Expr.Kind is
         when Template_Match_Capture =>
            declare
               Captured_Name : Text_Type := Expr.Node.As_Match_Capture.F_Captured.Text;
               Previous_Value : W_Object;
            begin
               --  This expression captures the result of the underlying
               --  expression and lets its value pass through.

               --  First, save any previous name capture for restoration,
               --  and store the new one.

               Push_Frame_Context;
               Top_Frame.Top_Context.Name_Captured :=
                 To_Unbounded_Text (Captured_Name);

               if Top_Frame.Symbols.Contains (Captured_Name) then
                  Previous_Value := Top_Frame.Symbols.Element (Captured_Name);
               end if;

               Evaluate_Expression (Expr.Match_Expr);

               if Top_Frame.Data_Stack.Last_Element /= Match_False then
                  Top_Frame.Symbols.Include (Captured_Name, Top_Object);
               else
                  --  For early reference, that name may have already been
                  --  captured. If we eneded up not having a match, it needs
                  --  to be removed, or replaced by the previous value.

                  if Previous_Value /= null then
                     Top_Frame.Symbols.Include (Captured_Name, Previous_Value);
                  elsif Top_Frame.Symbols.Contains (Captured_Name) then
                     Top_Frame.Symbols.Delete (Captured_Name);
                  end if;
               end if;

               Pop_Frame_Context;
               Run_Outer_Callback := False;
            end;

         when Template_Selector =>
            declare
               Suffix : T_Expr_Vectors.Vector;
            begin
               Handle_Selector (Expr, Suffix);
            end;

            --  Never match the result of a selection. Matching happened
            --  before, when evaluating the right operand. At this stage,
            --  we may also not be in the right match mode anymore (e.g.
            --  we don't know if we match a reference or a call result).
            Run_Outer_Callback := False;

         when Template_Binary_Expr =>
            --  The convention for "and" and "or" binary operators is to push to
            --  the stack the last object that matched, otherwise false. This
            --  allows to capture that object later on, which can be useful for
            --  example if that object is a newly allocated one.

            declare
               Left, Right : W_Object;
            begin
               Left := Evaluate_Expression (Expr.Binary_Left);

               if Expr.Node.As_Binary_Expr.F_Op.Kind = Template_Operator_And then
                  if Left /= Match_False then
                     Right := Evaluate_Expression (Expr.Binary_Right);

                     if Right /= Match_False then
                        Push_Object (Right);
                     else
                        Push_Match_False;
                     end if;
                  else
                     Push_Match_False;
                  end if;

                  Run_Outer_Callback := False;
               elsif Expr.Node.As_Binary_Expr.F_Op.Kind = Template_Operator_Or then
                  if Left /= Match_False then
                     Push_Object (Left);
                  else
                     Right := Evaluate_Expression (Expr.Binary_Right);

                     if Right /= Match_False then
                        Push_Object (Right);
                     else
                        Push_Match_False;
                     end if;
                  end if;

                  Run_Outer_Callback := False;
               elsif Expr.Node.As_Binary_Expr.F_Op.Kind = Template_Operator_Amp then
                  Right := Evaluate_Expression (Expr.Binary_Right);

                  if Left.Dereference.all in W_Text_Expression_Type'Class
                    and then Right.Dereference.all in W_Text_Expression_Type'Class
                  then
                     declare
                        Container : W_Text_Vector := new W_Text_Vector_Type;
                     begin
                        Container.A_Vector.Append (Left);
                        Container.A_Vector.Append (Right);

                        Push_Object (Container);
                     end;
                  else
                     declare
                        Container : W_Vector := new W_Vector_Type;
                     begin
                        Container.A_Vector.Append (Left);
                        Container.A_Vector.Append (Right);

                        Push_Object (Container);
                     end;
                  end if;
               end if;
            end;

         when Template_Unary_Expr =>
            declare
               Right : W_Object :=
                 Evaluate_Expression (Expr.Unary_Right).Dereference;
            begin
               if Expr.Node.As_Unary_Expr.F_Op.Kind = Template_Operator_Not then
                  if Right = Match_False then
                     Push_Match_True (Top_Object.Dereference);
                  else
                     Push_Match_False;
                  end if;
               end if;
            end;

         when Template_Literal =>
            if Expr.Node.Text = "true" then
               Push_Match_True (Top_Object);
            elsif Expr.Node.Text = "false" then
               Push_Match_False;
            else
               Error ("unkown literal '" & Expr.Node.Text & "'");
            end if;

         when Template_Token_Identifier | Template_Identifier =>
            Handle_Identifier (Expr.Node);

         when Template_Number =>
            Push_Object (W_Object'(new W_Integer_Type'(Value => Expr.Number)));

         when Template_Str =>
            Analyze_Replace_String (Expr);

            Push_Object (W_Object'(new W_Regexp_Type'(Value => Pop_Object)));

         when Template_Call_Expr =>
            Handle_Call (Expr);

            --  Prepare the matching context for the resulting value.
            --  As we're on a call match, we can
            --  change the context without pushing / popping (there's nothing
            --  else).

            if Top_Frame.Top_Context.Match_Mode = Match_Ref_Default then
               Top_Frame.Top_Context.Match_Mode := Match_Call_Default;
            end if;

         when Template_Lambda_Expr =>
            declare
               A_Lambda : W_Lambda := new W_Lambda_Type;
            begin
               Capture_Lambda_Environment (A_Lambda, Expr);
               Push_Object (A_Lambda);

               Pop_Error_Location;
            end;

         when Template_New_Expr =>
            if Top_Frame.Top_Context.An_Allocate_Callback /= null then
               Handle_New (Expr.Tree);
            else
               Push_Match_False;
            end if;

            Push_Frame_Context;
            Top_Frame.Top_Context.Match_Mode := Match_Has;
            Do_Pop_Frame_Context := True;

         when Template_At_Ref =>
            if Top_Frame.Top_Context.Left_Value = null then
               Error ("no left value available in this context");
            else
               Push_Object (Top_Frame.Top_Context.Left_Value);
            end if;

         when Template_Qualified_Match =>
            --  We are on an expression like has'something or is'something.
            --  Specify the kind of match we need to make, which will override
            --  the default.

            if Top_Frame.Top_Context.Match_Mode = Match_None then
               Error
                 ("qualified match operators only available in match context");
            end if;

            Push_Frame_Context;
            Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;

            if Expr.Node.As_Qualified_Match.F_Op = Template_Operator_Is then
               Top_Frame.Top_Context.Match_Mode := Match_Is;
            else
               Top_Frame.Top_Context.Match_Mode := Match_Has;
            end if;

            Evaluate_Expression (Expr.Qualified_Match_Expr);

            Pop_Frame_Context;

            Run_Outer_Callback := False;

         when others =>
            Error
              ("unexpected expression node kind: '"
               & Expr.Node.Kind'Wide_Wide_Image & "'");
      end case;

      if Run_Outer_Callback
        and then Top_Frame.Top_Context.Outer_Expr_Callback /= null
      then
         Top_Frame.Top_Context.Outer_Expr_Callback.all;
      end if;

      if Do_Pop_Frame_Context then
         Pop_Frame_Context;
      end if;

      Pop_Error_Location;
   end Evaluate_Expression;

   ----------------------------
   -- Analyze_Replace_String --
   ----------------------------

   Expression_Unit_Number : Integer := 1;

   procedure Analyze_Replace_String
     (Expr          : T_Expr;
      On_Group      : access procedure (Index : Integer; Value : W_Object) := null;
      On_Expression : access procedure (Expr : T_Expr) := null)
   is
      Result : W_Text_Vector := new W_Text_Vector_Type;
      New_Text : W_String;

      procedure Append_Text (Text : Text_Type) is
      begin
         New_Text := new W_String_Type;
         New_Text.Value := To_Unbounded_Text (Text);

         Result.A_Vector.Append (W_Object (New_Text));
      end Append_Text;

      procedure On_Error
        (Message : Text_Type; Filename : String; Loc : Source_Location)
      is
      begin
         Push_Error_Location
           (Expr.Node.Unit.Get_Filename,
            (Expr.Node.Sloc_Range.Start_Line,
             Expr.Node.Sloc_Range.Start_Column));

         Put_Line
           (To_Text (Get_Sloc_Str)
            & ": " & Message);

         raise Wrapping_Error;
      end On_Error;

      Prev_Error : Error_Callback_Type;
   begin
      Prev_Error := Error_Callback;
      Error_Callback := On_Error'Unrestricted_Access;

      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode := Match_None;
      Top_Frame.Top_Context.Is_Root_Selection := True;

      for Str of Expr.Str loop
         case Str.Kind is
            when Str_Kind =>
               Result.A_Vector.Append
                 (new W_String_Type'(Value => Str.Value));
            when Expr_Kind =>
               if On_Expression /= null then
                  On_Expression.All (Str.Expr);
               else
                  Evaluate_Expression (Str.Expr);
                  Result.A_Vector.Append (W_Object (Pop_Object));
               end if;
            when Group_Kind =>
               declare
                  Value : W_Object :=
                    Top_Frame.Matched_Groups.Element (Str.Group_Number);
               begin
                  if On_Group /= null then
                     On_Group.all (Str.Group_Number, Value);
                  else
                     Append_Text (Value.To_String);
                  end if;
               end;
         end case;
      end loop;

      Push_Object (Result);
      Error_Callback := Prev_Error;
      Pop_Frame_Context;
   end Analyze_Replace_String;

   function Push_Global_Identifier (Name : Text_Type) return Boolean is
      A_Module : T_Module;
      Tentative_Symbol : W_Object;
      A_Semantic_Entity : T_Entity;

      Implicit_New  : W_Object;
   begin
      if Name = "self" then
         Push_Object (Get_Implicit_Self);

         return True;
      elsif Name = "new" then
         Implicit_New := Get_Implicit_New;

         if Implicit_New /= null then
            Push_Object (Get_Implicit_New);

            return True;
         end if;
      elsif Name = "text" then
         --  We're on an object to text conversion. Set the runtime object.
         --  When running the call, the link with the undlerlying expression
         --  will be made.
         Push_Function (null, Call_Convert_To_Text'Access);

         return True;
      elsif Name = "string" then
         --  We're on an object to string conversion. Set the text object.
         --  When running the call, the actual text value will be computed and
         --  put in the object.
         --Push_Object (W_Object'(new W_String_Type));

         Error ("not implemented"); -- How about a kind in text_conversion?

         return True;
      elsif Name = "normalize_ada_name" then
         Push_Function (null, Call_Normalize_Ada_Name'Access);
         return True;
      elsif Name = "replace_text" then
         Push_Function (null, Call_Replace_Text'Access);
         return True;
      elsif Name = "to_lower" then
         Push_Function (null, Call_To_Lower'Access);
         return True;
      elsif Name = "unindent" then
         Push_Function (null, Call_Unindent'Access);
         return True;
      end if;

      -- Check in the dynamic symols in the frame

      Tentative_Symbol := Get_Visible_Symbol (Top_Frame.all, Name);

      A_Module := Get_Module (Top_Frame.all);

      --  Check if the current module is the name we're looking for

      if To_Text (A_Module.Name) = Name then
         Push_Object
           (W_Object'(new W_Static_Entity_Type'
              (An_Entity => T_Entity (A_Module))));
         return True;
      end if;

      --  Check in the static symbols in the module

      if A_Module.Children_Indexed.Contains (Name) then
         if Tentative_Symbol = null then
            A_Semantic_Entity := A_Module.Children_Indexed (Name);

            if A_Semantic_Entity.all in T_Template_Type'Class then
               Tentative_Symbol := new W_Static_Entity_Type'
                 (An_Entity => A_Semantic_Entity);
            end if;
         else
            Error ("can't reference " & Name & ", multiple definitions hiding");
         end if;
      end if;

      -- Check in the imported symbols in the module

      for Imported of A_Module.Imported_Modules loop
         if Imported.Children_Indexed.Contains (Name) then
            if Tentative_Symbol = null then
               A_Semantic_Entity := Imported.Children_Indexed (Name);

               if A_Semantic_Entity.all in T_Template_Type'Class then
                  Tentative_Symbol := new W_Static_Entity_Type'
                    (An_Entity => A_Semantic_Entity);
               end if;
            else
               Error ("can't reference " & Name & ", multiple definitions hiding");
            end if;
         end if;
      end loop;

      -- Check in the namesaces symbols

      if Wrapping.Semantic.Analysis.Root.Children_Indexed.Contains (Name) then
         if Tentative_Symbol = null then
            A_Semantic_Entity :=
              Wrapping.Semantic.Analysis.Root.Children_Indexed.Element (Name);

            if A_Semantic_Entity.all in T_Namespace_Type'Class
              or else A_Semantic_Entity.all in T_Module_Type'Class
            then
               Tentative_Symbol := new W_Static_Entity_Type'
                 (An_Entity => A_Semantic_Entity);
            end if;
         else
            Error ("can't reference " & Name & ", multiple definitions hiding");
         end if;
      end if;

      if Tentative_Symbol = null then
         return False;
      else
         Push_Object (Tentative_Symbol);
         return True;
      end if;
   end Push_Global_Identifier;

   procedure Handle_Global_Identifier (Name : Text_Type) is
   begin
      if not Push_Global_Identifier (Name) then
         Error ("can't find global reference to '" & Name & "'");
      end if;
   end Handle_Global_Identifier;

   procedure Handle_Identifier (Node : Template_Node'Class) is
      procedure Handle_Language_Entity_Selection;

      procedure Handle_Static_Entity_Selection is
         Name : Text_Type := Node.Text;
      begin
         -- TODO: We probably don't need a specific function here anymore.

         if not Pop_Object.Push_Value (Name) then
            if Top_Frame.Top_Context.Match_Mode /= Match_None then
               Push_Match_False;
            else
               Error ("'" & Node.Text & "' not found");
            end if;
         end if;
      end Handle_Static_Entity_Selection;

      procedure Handle_Language_Entity_Selection is
         Name : Text_Type := Node.Text;

         Implicit_Self : W_Object;
         Implicit_New  : W_Object;

         Found_Self_Entity : Boolean;
         Found_New_Entity : Boolean;

         Self_Object : W_Object;
         Prefix_Entity : W_Object;
      begin
         -- We're resolving a reference to an entity

         Prefix_Entity := Top_Object.Dereference;

         if Top_Frame.Top_Context.Is_Root_Selection then
            --  If we're on the implicit entity, then first check if there's
            --  some more global identifier overriding it.

            if Push_Global_Identifier (Node.Text) then

               return;
            end if;

            --  Retreive the entity from implicit self. If Implicit new
            --  exist, we need to also attempt at retreiving its value.
            --  We'll return either the entity coming from one of the two,
            --  or raise an error if both contain such name.

            Implicit_Self := Get_Implicit_Self;
            Implicit_New := Get_Implicit_New;

            Found_Self_Entity := Implicit_Self.Push_Value (Name);

            if Implicit_New /= null then
               if not Found_Self_Entity then
                  if Implicit_New.Push_Value (Name) then
                     return;
                  end if;
               else
                  Self_Object := Pop_Object;

                  Found_New_Entity := Implicit_New.Push_Value (Name);

                  if not Found_New_Entity then
                     Push_Object (Self_Object);
                     return;
                  else
                     Error ("ambiguous reference to '" & Name & "' between self and new objects");
                  end if;
               end if;
            elsif Found_Self_Entity then
               return;
            end if;

            if Top_Frame.Top_Context.Match_Mode /= Match_None then
               Push_Match_False;
               return;
            else
               Error ("'" & Node.Text & "' not found");
            end if;
         else
            --  We're on an explicit name. Pop it and push the result.
            Pop_Object;

            if Prefix_Entity.Push_Value (Name) then
               --  We found a component of the entity and it has been pushed
               return;
            else
               if Top_Frame.Top_Context.Match_Mode /= Match_None then
                  Push_Match_False;
                  return;
               else
                  Error ("'" & Name & "' component not found");
               end if;
            end if;
         end if;
      end Handle_Language_Entity_Selection;

      Top : W_Object;
   begin
      if Top_Frame.Data_Stack.Length /= 0 then
         Top := Top_Object.Dereference;

         if Top.all in W_Static_Entity_Type'Class then
            Handle_Static_Entity_Selection;
         else
            Handle_Language_Entity_Selection;
         end if;
      else
         Handle_Global_Identifier (Node.Text);
      end if;
   end Handle_Identifier;

   procedure Handle_Call_Parameters
     (Args : T_Arg_Vectors.Vector;
      Name_For_Position : access function (Position : Integer) return Template_Node;
      Store_Param_Value : access procedure (Name_Node : Template_Node; Value : W_Object);
      Perpare_Param_Evaluation : access procedure (Name_Node : Template_Node; Position : Integer) := null)
   is
      Parameter_Index : Integer;
      Parameter_Value : W_Object;
      In_Named_Section : Boolean := False;
      Name_Node : Template_Node;
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Is_Root_Selection := True;
      Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;

      Parameter_Index := 1;

      for Param of Args loop
         if Param.Name /= "" then
            In_Named_Section := True;
            Name_Node := Param.Name_Node;
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;

            Name_Node := Name_For_Position.all (Parameter_Index);
         end if;

         if Perpare_Param_Evaluation /= null then
            Perpare_Param_Evaluation (Name_Node, Parameter_Index);
         end if;

         Evaluate_Expression (Param.Expr);

         Parameter_Value := Pop_Object;

         Store_Param_Value (Name_Node, Parameter_Value);

         Parameter_Index := Parameter_Index + 1;
      end loop;

      Pop_Frame_Context;
   end Handle_Call_Parameters;

   procedure Handle_Template_Call
     (A_Template_Instance : W_Template_Instance;
      Args : T_Arg_Vectors.Vector)
   is
      procedure Perpare_Param_Evaluation (Name_Node : Template_Node; Position : Integer) is
         New_Value : W_Object;
      begin
         Push_Frame_Context;

         if A_Template_Instance.Symbols.Contains (Name_Node.Text) then
            --  A variable is an indirection to a value. Return that value.

            Top_Frame.Top_Context.Left_Value :=
              A_Template_Instance.Symbols.Element (Name_Node.Text).Value;
         else
            --  Text are modeled as a container of texts. So by default, this is
            --  an empty container.
            --  TODO: we're only handling text types for now, but will need to
            --  handle new ones at some point.
            New_Value := new W_Text_Vector_Type;
            Top_Frame.Top_Context.Left_Value := New_Value;
         end if;
      end;

      function Name_For_Position (Position : Integer) return Template_Node is
      begin
         return A_Template_Instance.Defining_Entity.Get_Variable_For_Index
           (Position).Name_Node;
      end Name_For_Position;

      procedure Store_Param_Value (Name_Node : Template_Node; Value : W_Object) is
         Ref : W_Reference;
         A_Var : T_Var;
         Name : Text_Type := Name_Node.Text;
      begin
         A_Var := T_Var (A_Template_Instance.Defining_Entity.Get_Component (Name));

         if A_Template_Instance.Symbols.Contains (Name) then
            Ref := A_Template_Instance.Symbols.Element (Name);
         else
            Ref := new W_Reference_Type;
            A_Template_Instance.Symbols.Insert (Name, Ref);
         end if;

         --  The container is an indirection to a value. Remove the previous one
         --  and add the new one instead.

         if A_Var = null then
            --  In that case, there's no type to refer to. Just associate the value.
            --  This is the case for intrinsic variables created for types.
            --  TODO: It'd be better to have a Get_Type_For primitive on the
            --  defining entity, or something like that.
            Ref.Value := Value;
         elsif A_Var.Kind = Text_Kind then
            -- If we were provided with a text,
            --  reference it, otherwise add a conversion to text node. We can't
            --  resolve the value just yet as it may depend on dynamically computed
            --  data, such as lambda.
            --  TODO: This is effectively an implicit conversion to text. Document
            --  it, and make sure that it's only done in the case of text kind, not
            --  object kind.

            if Value.Dereference.all in W_Text_Expression_Type'Class then
               Ref.Value := Value;
            else
               Ref.Value := new W_Text_Conversion_Type'(An_Object => Value);
            end if;
         else
            Ref.Value := Value;
         end if;

         Pop_Frame_Context;
      end Store_Param_Value;
   begin
      Handle_Call_Parameters
        (Args,
         Name_For_Position'Access,
         Store_Param_Value'Access,
         Perpare_Param_Evaluation'Access);
   end Handle_Template_Call;

   procedure Handle_Visitor_Call
     (An_Entity : W_Node;
      A_Visitor : T_Visitor;
      Args      : T_Arg_Vectors.Vector)
   is
      Symbols : W_Object_Maps.Map;

      function Name_For_Position (Position : Integer) return Template_Node is
      begin
         return A_Visitor.Arguments_Ordered.Element (Position).Name_Node;
      end Name_For_Position;

      procedure Store_Param_Value (Name_Node : Template_Node; Value : W_Object) is
      begin
         Symbols.Insert (Name_Node.Text, Value);
      end Store_Param_Value;

      function Sub_Visitor
        (E : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action
      is
         A_Visit_Action : Visit_Action := Into;
      begin
         Result := null;

         Apply_Wrapping_Program
           (W_Node (E),
            A_Visitor,
            A_Visit_Action);

         return A_Visit_Action;
      end Sub_Visitor;

      Prev_Visit_Id : Integer;
      A_Visit_Action : Visit_Action := Unknown;
      --Traverse_Result : W_Object;
   begin
      --  Increment the visitor counter and set the current visitor id, as to
      --  track entities that are being visited by this iteration.
      Prev_Visit_Id := Current_Visitor_Id;
      Visitor_Counter := Visitor_Counter + 1;
      Current_Visitor_Id := Visitor_Counter;

      -- The analysis needs to be done within the previous frame (in particular
      -- to get capture groups) then all valuated symbols needs to be allocated
      -- to the next frame (in particular to be destroyed when popping).

      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode := Match_None;

      Handle_Call_Parameters
        (Args,
         Name_For_Position'Access,
         Store_Param_Value'Access);

      Pop_Frame_Context;

      Push_Frame (A_Visitor);

      Top_Frame.Symbols.Move (Symbols);

      -- Apply_Wrapping_Program will push its own frame, which is local to the
      -- actual entity to be analyzed. The one pushed above is global to all
      -- calls and contains parameter evaluation, to be done only once.

      Apply_Wrapping_Program
        (An_Entity,
         A_Visitor,
         A_Visit_Action);

      if A_Visit_Action in Over | Stop then
         Error ("visit action from visitor to caller not implemented");
      end if;

      --  Reset the visitor id to the previous value, we're back to the
      --  enclosing visitor invocation.
      Current_Visitor_Id := Prev_Visit_Id;

      Pop_Frame;
   end Handle_Visitor_Call;

   procedure Handle_Call (Expr : T_Expr) is
      Called : W_Object;
      Result : W_Object;
   begin
      Push_Frame_Context;

      --  If we're matching, currently under the default ref mode, then move
      --  to the default call mode.

      if Top_Frame.Top_Context.Match_Mode = Match_Ref_Default then
         Top_Frame.Top_Context.Match_Mode := Match_Call_Default;
      end if;

      Push_Frame_Context;

      --  We want to use the outer expression callback when evaluating the
      --  parameters, in particular to support something like
      --     child (bla).all()
      --  where the child identifier is extracted through regular test, but the
      --  bla expression needs to be using whatever outer expression callback
      --  is set.
      Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;
      Evaluate_Expression (Expr.Called);
      Pop_Frame_Context;

      Called := Top_Object.Dereference;
      --  TODO: we can probably pop the called object now that we track the matching
      --  object.

      if Called = Match_False then
         if Top_Frame.Top_Context.Match_Mode /= Match_None then
            Pop_Frame_Context;
            Pop_Object; -- Pop call symbol
            Push_Match_False;
            return;
         else
            Error ("call not matching context");
         end if;
      end if;

      --  TODO: It's not clear at all that we need three different branches
      --  here, Push_Call_Result could be properly set for all of these objects.

      --  Within a call, if we're matching, fall back to default ref matching

      if Top_Frame.Top_Context.Match_Mode /= Match_None then
         Top_Frame.Top_Context.Match_Mode := Match_Ref_Default;
      end if;

      Top_Frame.Top_Context.Is_Root_Selection := True;

      Called.Push_Call_Result (Expr.Args);

      Result := Pop_Object;
      Pop_Object; -- Pop call symbol
      Push_Object (Result);

      Pop_Frame_Context;
   end Handle_Call;

   procedure Compute_Selector_Suffix (Suffix : in out T_Expr_Vectors.Vector) is
      Terminal : T_Expr;
   begin
      if Suffix.Length = 0 then
         return;
      end if;

      Terminal := Suffix.Last_Element;
      Suffix.Delete_Last;

      --  The left part of a selector may have calls. In this
      --  case, these calls are unrelated to the value that is
      --  possibly being captured. E.g. in:
      --     a: b ().c
      --  b () value is not being captured in a.
      --  In order to respect that, the current captured name is
      --  removed when processing the left part of the selector.
      --  Similarily, we only fold on the target of the fold. For
      --  example, in:
      --     child ().child ().fold ()
      --  the first child is a selecing the first match, the second
      --  is folded.
      --  Note that the left end of an expression is never matching
      --  with the outer context, hence setting the match flag to
      --  none.

      Push_Frame_Context;
      Top_Frame.Top_Context.Name_Captured := To_Unbounded_Text ("");
      Top_Frame.Top_Context.Is_Expanding_Context := False;
      Top_Frame.Top_Context.Match_Mode := Match_None;
      Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;

      for Suffix_Expression of Suffix loop
         Evaluate_Expression (Suffix_Expression);
         Top_Frame.Top_Context.Is_Root_Selection := False;
      end loop;

      Pop_Frame_Context;
      Push_Frame_Context;
      Top_Frame.Top_Context.Is_Root_Selection := False;

      --  Run the terminal separately. In particular in the case of:
      --     x.y.z.child().all()
      --  while x.y.z needs to be called outside of the expanding context,
      --  child () need to be called with the frame context set by all which
      --  set in particular expanding context to true.

      Evaluate_Expression (Terminal);
      Pop_Frame_Context;
   end Compute_Selector_Suffix;

   procedure Handle_Selector (Expr : T_Expr; Suffix : in out T_Expr_Vectors.Vector) is
   begin
      --  In a selector, we compute the left object, build the right
      --  expression based on the left object, and then put the result
      --  on the left object on the stack.

      if Expr.Selector_Left = null then
         --  This can happen in particualr with the rule "dotted_identifier".
         --  TODO: We should look at getting rid of this special case.

         Push_Frame_Context;
         Top_Frame.Top_Context.Is_Root_Selection := True;
         Evaluate_Expression (Expr.Selector_Right);
         Pop_Frame_Context;
      elsif Expr.Selector_Right.Kind in Template_All_Expr then
         Handle_All (Expr, Suffix);
      elsif Expr.Selector_Right.Kind in Template_Fold_Expr then
         Handle_Fold (Expr, Suffix);
      elsif Expr.Selector_Left.Kind in Template_Selector then
         Suffix.Prepend (Expr.Selector_Right);
         Handle_Selector (Expr.Selector_Left, Suffix);
      else
         Suffix.Prepend (Expr.Selector_Right);
         Suffix.Prepend (Expr.Selector_Left);

         Compute_Selector_Suffix (Suffix);
      end if;
   end Handle_Selector;

   procedure Handle_Fold (Selector : T_Expr;  Suffix : in out T_Expr_Vectors.Vector) is

      Fold_Expr : T_Expr := Selector.Selector_Right;
      Init_Value : W_Object;

      procedure Expand_Action is
      begin
         Evaluate_Expression (Fold_Expr.Combine);
      end Expand_Action;

   begin
      --  Inside the folded expression, we need to go back to a situation where
      --  self is top of the stack, as name can refer to the implicit self. Re
      --  push this value

      Push_Frame_Context;
      Top_Frame.Top_Context.Is_Root_Selection := True;
      Push_Implicit_Self (Get_Implicit_Self);
      Init_Value := Evaluate_Expression (Fold_Expr.Default);
      Pop_Object;
      Pop_Frame_Context;

      Push_Frame_Context;
      Top_Frame.Top_Context.Is_Expanding_Context := True;
      Top_Frame.Top_Context.Expand_Action := Expand_Action'Unrestricted_Access;
      Top_Frame.Top_Context.Match_Mode := Match_None;
      Top_Frame.Top_Context.Is_Root_Selection := True;

      Evaluate_Expression (Selector.Selector_Left);

      --  Keep the result of the evaluate expression. If the result is false,
      --  this means that nothing was actually found. In that case, the init
      --  value needs to be pused.
      --  TODO: This will not work for folding evaluating booleans, where
      --  the init value may be true, but the result is false.

      if Top_Frame.Data_Stack.Last_Element = Match_False then
         Pop_Object;
         Push_Object (Init_Value);
      end if;

      Pop_Frame_Context;

      Compute_Selector_Suffix (Suffix);
   end Handle_Fold;

   procedure Handle_All (Selector : T_Expr; Suffix : T_Expr_Vectors.Vector)
   is
      Initial_Context : Frame_Context := Top_Frame.Top_Context;

      procedure Expand_Action is
      begin
         --  Restore the context at this point of the call. This is important
         --  in particular if there was an expansion happening there,
         --  e.g. a.all().b.all().
         Push_Frame_Context (Initial_Context.all);

         --  The outer callback has to be a match check here. If it's a
         --  Outer_Expression_Pick, this is only to be called on the returning
         --  value.
         Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;

         for Suffix_Expression of Suffix loop
            Evaluate_Expression (Suffix_Expression);
         end loop;

         --  Execute the outer action once per run of the suffix, which may be
         --  a Outer_Expression_Pick call.
         Initial_Context.Outer_Expr_Callback.all;
         Pop_Frame_Context;
      end Expand_Action;

   begin
      Push_Frame_Context;

      Top_Frame.Top_Context.Expand_Action := Expand_Action'Unrestricted_Access;
      Top_Frame.Top_Context.Outer_Expr_Callback := Outer_Expression_Match'Access;
      Top_Frame.Top_Context.Is_Expanding_Context := True;
      Top_Frame.Top_Context.Match_Mode := Match_None;

      Evaluate_Expression (Selector.Selector_Left);

      Pop_Frame_Context;
   end Handle_All;

   procedure Handle_New (Create_Tree : T_Create_Tree) is

      function Handle_Create_Template (New_Tree : T_Create_Tree)
         return W_Template_Instance
      is
         A_Template_Instance : W_Template_Instance;
         Captured : Text_Type := To_Text (New_Tree.Capture_Name);
      begin
         A_Template_Instance := Create_Template_Instance
           (null, New_Tree.A_Template);

         if Captured /= "" then
            Top_Frame.Symbols.Include
              (Captured, W_Object (A_Template_Instance));
         end if;

         Push_Implicit_New (A_Template_Instance);
         Handle_Template_Call (A_Template_Instance, New_Tree.Args);
         Pop_Object;

         return A_Template_Instance;
      end Handle_Create_Template;

      function Handle_Create_Tree
        (A_Tree : T_Create_Tree;
         Parent : W_Template_Instance) return W_Template_Instance
      is
         Main_Node : W_Template_Instance;
         Dummy : W_Template_Instance;
      begin
         if A_Tree.A_Template /= null then
            Main_Node := Handle_Create_Template (A_Tree);

            if Parent = null then
               --  If this is the root of the creation, then we need to signal this
               --  outside. This is needed in particular for constructions like:
               --  child (new (T ())) or child (new ([T(), T()])) where the root
               --  entities need to be connected to the children of the parent entity.

               --  for the form new (T() []), only the first one needs to be
               --  passed above.

               Top_Frame.Top_Context.An_Allocate_Callback.all (Main_Node);
            else
               Add_Child (Parent, Main_Node);
            end if;

            for C of A_Tree.Subtree loop
               Dummy := Handle_Create_Tree (C, Main_Node);
            end loop;

            return Main_Node;
         else
            --  In the case of new ([T(), T()], every first level will need to
            --  be passed to the above level, and the last one will be the
            --  result of the operator.

            for C of A_Tree.Subtree loop
               Main_Node := Handle_Create_Tree (C, Parent);
            end loop;

            return Main_Node;
         end if;
      end Handle_Create_Tree;

   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode := Match_None;

      Push_Allocated_Entity (Handle_Create_Tree (Create_Tree, null));

      Pop_Frame_Context;
   end Handle_New;

   procedure Capture_Lambda_Environment (A_Lambda : W_Lambda; Expr : T_Expr) is
   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Is_Root_Selection := True;

      for Name of Expr.Lambda_Closure loop
         if Push_Global_Identifier (Name) then
            --  If the object is an imlicit ref, it may be marked self
            --  or new. We don't want to carry this property over to the lambda
            --  call, so remove it.

            if Top_Object.Dereference.all in W_Static_Entity_Type then
               --  We don't capture static references, they can later be
               --  retreived from context. Genreating symbols for them would
               --  also confused name resolution as we would have a symbol
               --  and a statically solvable name.
               Pop_Object;
            elsif Top_Object.all in W_Reference_Type'Class then
               A_Lambda.Captured_Symbols.Insert
                 (Name, new W_Reference_Type'
                    (Value => W_Reference (Pop_Object).Value, others => <>));
            else
               A_Lambda.Captured_Symbols.Insert (Name, Pop_Object);
            end if;
         end if;
      end loop;

      A_Lambda.Expr := Expr.Lambda_Expr;
      A_Lambda.Implicit_Self := W_Node (Get_Implicit_Self);
      A_Lambda.Implicit_New := W_Node (Get_Implicit_New);
      A_Lambda.Lexical_Scope := Top_Frame.Lexical_Scope;
   end Capture_Lambda_Environment;

   procedure Run_Lambda (A_Lambda : W_Lambda_Type) is
      Copy_Symbols : W_Object_Maps.Map;
      Result : W_Object;
   begin
      Push_Frame (A_Lambda.Lexical_Scope);

      Copy_Symbols := A_Lambda.Captured_Symbols.Copy;
      Top_Frame.Symbols.Move (Copy_Symbols);

      if A_Lambda.Implicit_Self /= null then
         Push_Implicit_Self (A_Lambda.Implicit_Self);
      end if;

      if A_Lambda.Implicit_New /= null then
         Push_Implicit_New (A_Lambda.Implicit_New);
      end if;

      Result := Evaluate_Expression (A_Lambda.Expr);
      Pop_Frame;
      Push_Object (Result);
   end Run_Lambda;

   procedure Outer_Expression_Match is
   begin
      if Top_Frame.Top_Context.Match_Mode not in Match_None | Match_Has then
         --  If we're matching, and we're not forcing the "has" mode, then check
         --  that the initial object we had on the stack matches the new one.

         if not Top_Frame.Top_Context.Outer_Object.Match_With_Top_Object then
            Pop_Object;
            Push_Match_False;
         end if;
      end if;
   end Outer_Expression_Match;

   procedure Outer_Expression_Pick is
      --  TODO: Visit action probably to be controlled on the frame here.
      A_Visit_Action : Visit_Action := Into;
   begin
      Handle_Command (Top_Frame.Top_Context.Current_Command, A_Visit_Action);
   end Outer_Expression_Pick;

end Wrapping.Runtime.Analysis;
