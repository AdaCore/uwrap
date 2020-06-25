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
   procedure Handle_Call (Node : Call_Expr'Class);
   procedure Handle_Template_Call (A_Template_Instance : W_Template_Instance; Args : Argument_List);
   procedure Handle_Visitor_Call
     (An_Entity : W_Node;
      A_Visitor : T_Visitor;
      Args : Argument_List;
      Apply_To_All : Boolean);
   procedure Handle_Fold (Folded_Expression : Template_Node'Class; Node : Fold_Expr'Class);
   procedure Handle_New (Node : Create_Template_Tree'Class);

   procedure Analyze_Replace_String
     (Node : Template_Node'Class;
      On_Group : access procedure (Index : Integer; Value : W_Object) := null;
      On_Expression : access procedure (Expression : Template_Node) := null);

   procedure Build_Lambda (A_Lambda : W_Lambda; Lambda_Expression : Template_Node);

   procedure Call_Convert_To_Text
     (Object : access W_Object_Type'Class;
      Params : Argument_List)
   is
   begin
      if Params.Children_Count = 1 then
         Push_Object
           (W_Object'
              (new W_Text_Conversion_Type'
                   (An_Object => Evaluate_Expression (Params.Child (1)))));
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
      Top_Frame.Top_Context := new Frame_Context_Type'
        (Parent_Context       => Top_Frame.Top_Context,
         Match_Mode           => Top_Frame.Top_Context.Match_Mode,
         Is_Folding_Context   => Top_Frame.Top_Context.Is_Folding_Context,
         Name_Captured        => Top_Frame.Top_Context.Name_Captured,
         Folding_Expression   => Top_Frame.Top_Context.Folding_Expression,
         An_Allocate_Callback => Top_Frame.Top_Context.An_Allocate_Callback,
         Left_Value           => Top_Frame.Top_Context.Left_Value,
         Is_Root_Selection    => Top_Frame.Top_Context.Is_Root_Selection,
         Matching_Object      => Top_Frame.Top_Context.Matching_Object);
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

   procedure Apply_Wrapping_Program
     (A_Language_Entity : W_Node;
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
        (A_Command  : T_Command;
         Expression : Template_Node'Class);
      --  This will create a template instance from the clause information,
      --  setting parameter expression and adding it to the relevant language
      --  object.

      procedure Create_And_Set_Template_Instance
        (A_Command : T_Command;
         Expression : Template_Node'Class)
      is
      begin
         Evaluate_Expression (Expression);
      end Create_And_Set_Template_Instance;

      procedure Apply_Template_Action
        (A_Language_Entity : W_Node; Template_Clause : T_Weave_Or_Wrap)
      is
         A_Template_Instance : W_Template_Instance;
         Self_Weave : Boolean := False;
         Result : W_Object;
      begin
         Push_Error_Location (Template_Clause.Node);

         if Template_Clause.Is_Null then
            --  We've set a null template - the objective is to prevent this
            --  entity to be wrapped by this template.

            if Template_Clause.Arguments.Children_Count /= 1 then
               Error ("expected one argument to null");
            end if;

            Evaluate_Expression (Template_Clause.Arguments.Child (1).As_Argument.F_Value);
            Result := Pop_Object.Dereference;

            if Result.all not in W_Static_Entity_Type'Class
              or else W_Static_Entity (Result).An_Entity.all
                not in T_Template_Type'Class
            then
               Error ("expected template reference");
            end if;

            A_Language_Entity.Forbidden_Template_Names.Include
              (T_Template (W_Static_Entity (Result).An_Entity).Full_Name);

            --  TODO: remove the template if it's already been created in the
            --  context of a weave clause
         else
            if Template_Clause.Call_Reference = null then
               --  No name to the call, that means that we're expecting to self-weave
               --  the current template.
               if Template_Clause.all not in Weave_Type'Class then
                  Error ("self wrap not allowed, either weave or provide a template or visitor name");
               elsif A_Language_Entity.all in W_Template_Instance_Type'Class then
                  A_Template_Instance := W_Template_Instance (A_Language_Entity);
               else
                  Error ("only template instances can be self weaved");
               end if;

               Self_Weave := True;
            elsif Template_Clause.Call_Reference.all in T_Template_Type'Class then
               A_Template_Instance := A_Language_Entity.Get_Template_Instance
                 (T_Template (Template_Clause.Call_Reference));

               if (Template_Clause.all in Weave_Type'Class
                   or else A_Template_Instance = null
                   or else A_Template_Instance.Is_Wrapping = False)
                 and then not A_Language_Entity.Forbidden_Template_Names.Contains
                      (Template_Clause.Call_Reference.Full_Name)
               then
                  if A_Template_Instance = null then
                     A_Template_Instance := A_Language_Entity.Create_Template_Instance
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


               Handle_Template_Call (A_Template_Instance, Template_Clause.Arguments);

               if not Self_Weave then
                  Pop_Object;
               end if;
            end if;
         end if;

         Pop_Error_Location;
      end Apply_Template_Action;

      procedure Apply_Command (A_Command : T_Command) is
         Matched : Boolean;
      begin
         --  The command is the enclosing scope for all of its clauses. It
         --  will in particular receive the matching groups and the temporary
         --  values that can be used consistently in the various clauses
         Push_Frame (A_Command);
         Push_Implicit_Self (A_Language_Entity);

         Top_Frame.Top_Context.An_Allocate_Callback := Allocate'Unrestricted_Access;

         if not A_Command.Match_Expression.Is_Null then
            Top_Frame.Top_Context.Match_Mode := Match_Ref_Default;

            Top_Frame.Top_Context.Matching_Object := W_Object
              (A_Language_Entity);

            Evaluate_Expression (A_Command.Match_Expression);
            Matched := Pop_Object /= Match_False;
            Top_Frame.Top_Context.Match_Mode := Match_None;
         else
            Matched := True;
         end if;

         if Matched then
            if A_Command.Template_Clause /= null then
               -- HANDLE WEAVE OR WRAP

               if A_Command.Template_Clause.A_Visit_Action /= Unknown then
                  -- TODO: consider differences between weave and wrap here
                  --  TODO: This doesn't consider different visits, each
                  --  should have its own decision
                  if not A_Language_Entity.Traverse_Decision_Taken then
                     A_Language_Entity.Traverse_Decision_Taken := True;
                     A_Visit_Action := A_Command.Template_Clause.A_Visit_Action;
                  end if;
               else
                  declare
                     Entity_Target : W_Node;
                     Tmp_Target : W_Object;
                  begin
                     if not A_Command.Template_Clause.Target_Object.Is_Null then
                        Evaluate_Expression (A_Command.Template_Clause.Target_Object);
                        Tmp_Target := Pop_Object.Dereference;

                        if Tmp_Target = Match_False then
                           Error ("could not select object to wrap or weave");
                        elsif Tmp_Target.all in W_Node_Type'Class then
                           Entity_Target := W_Node (Tmp_Target);
                        elsif Tmp_Target.all in W_Static_Entity_Type'Class then
                           Entity_Target := W_Node
                             (Get_Object_For_Entity
                                (W_Static_Entity (Tmp_Target).An_Entity));
                        else
                           Error ("can't wrap or weave selected object "
                                  & Wide_Wide_Expanded_Name (Tmp_Target.all'Tag));
                        end if;

                        --  A number of the function below assume that
                        -- children for the entity are set - make sure that
                        -- they are
                        W_Node_Type'Class (Entity_Target.all).Pre_Visit;
                     else
                        Entity_Target := A_Language_Entity;
                     end if;

                     if A_Command.Template_Clause.Call_Reference /= null
                       or else not A_Command.Template_Clause.Arguments.Is_Null
                     then
                        -- There is an explicit template call. Pass this on either the
                        -- current template or the whole tree

                        if A_Command.Template_Clause.Call_Reference/= null
                          and then A_Command.Template_Clause.Call_Reference.all in T_Visitor_Type'Class
                        then
                           Handle_Visitor_Call
                             (Entity_Target,
                              T_Visitor (A_Command.Template_Clause.Call_Reference),
                              A_Command.Template_Clause.Arguments,
                              A_Command.Template_Clause.Is_All);
                        elsif A_Command.Template_Clause.Is_All then
                           -- TODO: This could all be moved to Handle Template Call instead, and
                           -- would be consistent with the fact that everything above is
                           -- in Handle_Visitor_Call
                           declare
                              function Apply_Template
                                (E      : access W_Object_Type'Class;
                                 Result : out W_Object) return Visit_Action
                              is
                              begin
                                 Result := null;

                                 Apply_Template_Action (W_Node (E), A_Command.Template_Clause);

                                 return Into;
                              end Apply_Template;

                              Traverse_Result : W_Object;
                           begin
                              A_Visit_Action := Entity_Target.Traverse
                                (A_Mode       => Child_Depth,
                                 Include_Self => True,
                                 Final_Result => Traverse_Result,
                                 Visitor      => Apply_Template'Access);
                           end;
                        else
                           Apply_Template_Action (Entity_Target, A_Command.Template_Clause);
                        end if;
                     else
                        if A_Command.Template_Clause.Is_All then
                           --  There is no template call. Apply the current
                           --  program to all children. Avoid to apply it on the
                           --  current entity which is already being analyzed
                           --  under the current program.

                           for E of Entity_Target.Children_Ordered loop
                              declare
                                 Dummy_Action : Visit_Action;
                                 Prev_Visit_Id : Integer;
                                 Traverse_Result : W_Object;
                              begin
                                 --  Increment the visitor counter and set the current visitor id, as to
                                 --  track entities that are being visited by this iteration.
                                 Prev_Visit_Id := Current_Visitor_Id;
                                 Visitor_Counter := Visitor_Counter + 1;
                                 Current_Visitor_Id := Visitor_Counter;

                                 -- TODO: This only works for the main program.
                                 -- implement support in case this is called
                                 -- from a visitor (the visitor should be
                                 -- traversed, not the main program.

                                 Dummy_Action := E.Traverse
                                   (Child_Depth, True, Traverse_Result, Analyze_Visitor'Access);

                                 Current_Visitor_Id := Prev_Visit_Id;
                              end;
                           end loop;
                        else
                           -- TODO: APPLY THE CURRENT PROGRAM ON THE TARGET ONLY
                           null;
                        end if;
                     end if;
                  end;
               end if;
            elsif A_Command.Nested_Actions /= null then
               Apply_Wrapping_Program
                 (A_Language_Entity,
                  A_Command.Nested_Actions,
                  A_Visit_Action);
            elsif not A_Command.Traverse_Expression.Is_Null then
               if not A_Language_Entity.Traverse_Decision_Taken then
                  A_Language_Entity.Traverse_Decision_Taken := True;

                  Evaluate_Expression (A_Command.Traverse_Expression);

                  if Top_Object.Dereference.all in W_Traverse_Decision_Type'Class then
                     --  In this case, a decision was taken as to the continuation of the
                     --  iteration.

                     declare
                        A_Decision : W_Traverse_Decision := W_Traverse_Decision
                          (Top_Object.Dereference);
                     begin
                        Pop_Object;

                        if A_Decision.A_Visit_Action = Over then
                           A_Visit_Action := Over;
                        else
                           if A_Decision.Into_Expression.Is_Null then
                              A_Visit_Action := Into;
                           else
                              Error ("traverse into functions not yet implemented");

                              A_Visit_Action := Over;
                           end if;
                        end if;
                     end;
                  end if;
               end if;
            end if;
         else
            if A_Command.Else_Actions /= null then
               if A_Command.Else_Actions.all in T_Command_Type'Class then
                  Apply_Command (T_Command (A_Command.Else_Actions));
               else
                  Apply_Wrapping_Program
                    (A_Language_Entity,
                     A_Command.Else_Actions,
                     A_Visit_Action);
               end if;
            end if;
         end if;

         Pop_Object;

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
              (A_Language_Entity, Wrapping_Entity, A_Visit_Action);
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

   function Evaluate_Expression (Node : Template_Node'Class) return W_Object is
   begin
      Evaluate_Expression (Node);

      return Pop_Object;
   end Evaluate_Expression;

   procedure Evaluate_Expression (Node : Template_Node'Class)
   is
      --  Some expression need to match with the outer object. For example:
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
      --  TODO: Review all cases of disconnection, e.g. &, not, a: etc.
      To_Match : Boolean := True;
   begin
      Push_Error_Location (Node);

      case Node.Kind is
         when Template_Match_Capture =>

            --  This expression captures the result of the underlying
            --  expression and lets its value pass through.

            --  First, save any previous name capture for restoration,
            --  and store the new one.

            Push_Frame_Context;
            Top_Frame.Top_Context.Name_Captured :=
              To_Unbounded_Text (Node.As_Match_Capture.F_Captured.Text);

            Evaluate_Expression (Node.As_Match_Capture.F_Expression);

            if Top_Frame.Data_Stack.Last_Element /= Match_False then
               Top_Frame.Symbols.Include
                 (Node.As_Match_Capture.F_Captured.Text,
                  Top_Frame.Data_Stack.Last_Element);
            else
               --  For early reference, that name may have already been
               --  captured. If we eneded up not having a match, it needs
               --  to be removed.

               if Top_Frame.Symbols.Contains
                 (Node.As_Match_Capture.F_Captured.Text)
               then
                  Top_Frame.Symbols.Delete
                    (Node.As_Match_Capture.F_Captured.Text);
               end if;
            end if;

            Pop_Frame_Context;
            To_Match := False;

         when Template_Selector =>
            --  In a selector, we compute the left object, build the right
            --  expression based on the left object, and then put the result
            --  on the left object on the stack.

            if not Node.As_Selector.F_Left.Is_Null then
               if Node.As_Selector.F_Right.Kind = Template_Fold_Expr then
                  --  Folding needs to be handled speparately, as the prefix
                  --  needs to be handled in folding context, with all
                  --  processing expressions properly set.

                  Handle_Fold (Node.As_Selector.F_Left, Node.As_Selector.F_Right.As_Fold_Expr);
               else
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
                  Top_Frame.Top_Context.Is_Folding_Context := False;
                  Top_Frame.Top_Context.Match_Mode := Match_None;

                  Evaluate_Expression (Node.As_Selector.F_Left);
                  Pop_Frame_Context;

                  Push_Frame_Context;
                  Top_Frame.Top_Context.Is_Root_Selection := False;

                  --  Leave the result of the previous expression on the stack,
                  --  it's suppposed to be consume by the next expression

                  Evaluate_Expression (Node.As_Selector.F_Right);

                  Pop_Frame_Context;
               end if;
            else
               Push_Frame_Context;
               Top_Frame.Top_Context.Is_Root_Selection := True;
               Evaluate_Expression (Node.As_Selector.F_Right);
               Pop_Frame_Context;
            end if;

            --  Never match the result of a selection. Matching happened
            --  before, when evaluating the right operand. At this stage,
            --  we may also not be in the right match mode anymore (e.g.
            --  we don't know if we match a reference or a call result).
            To_Match := False;

         when Template_Binary_Expr =>
            --  The convention for "and" and "or" binary operators is to push to
            --  the stack the last object that matched, otherwise false. This
            --  allows to capture that object later on, which can be useful for
            --  example if that object is a newly allocated one.

            declare
               Left, Right : W_Object;
            begin
               Left := Evaluate_Expression (Node.As_Binary_Expr.F_Lhs);

               if Node.As_Binary_Expr.F_Op.Kind = Template_Operator_And then
                  if Left /= Match_False then
                     Right := Evaluate_Expression (Node.As_Binary_Expr.F_Rhs);

                     if Right /= Match_False then
                        Push_Object (Right);
                     else
                        Push_Match_False;
                     end if;
                  else
                     Push_Match_False;
                  end if;

                  To_Match := False;
               elsif Node.As_Binary_Expr.F_Op.Kind = Template_Operator_Or then
                  if Left /= Match_False then
                     Push_Object (Left);
                  else
                     Right := Evaluate_Expression (Node.As_Binary_Expr.F_Rhs);

                     if Right /= Match_False then
                        Push_Object (Right);
                     else
                        Push_Match_False;
                     end if;
                  end if;

                  To_Match := False;
               elsif Node.As_Binary_Expr.F_Op.Kind = Template_Operator_Amp then
                  Right := Evaluate_Expression (Node.As_Binary_Expr.F_Rhs);

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
                 Evaluate_Expression (Node.As_Unary_Expr.F_Rhs).Dereference;
            begin
               if Node.As_Unary_Expr.F_Op.Kind = Template_Operator_Not then
                  if Right = Match_False then
                     Push_Match_True (Top_Object.Dereference);
                  else
                     Push_Match_False;
                  end if;
               end if;
            end;

         when Template_Literal =>
            if Node.Text = "true" then
               Push_Match_True (Top_Object);
            elsif Node.Text = "false" then
               Push_Match_False;
            else
               Error ("unkown literal '" & Node.Text & "'");
            end if;

         when Template_Token_Identifier | Template_Identifier =>
            Handle_Identifier (Node);

         when Template_Number =>
            declare
               Val : W_Integer := new W_Integer_Type;
            begin
               Val.Value := Integer'Wide_Wide_Value (Node.Text);

               Push_Object (Val);
            end;

         when Template_Str =>
            Analyze_Replace_String (Node);

            Push_Object (W_Object'(new W_Regexp_Type'(Value => Pop_Object)));

         when Template_Call_Expr =>
            Handle_Call (Node.As_Call_Expr);

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
               Build_Lambda (A_Lambda, Node.As_Lambda_Expr.F_Expression);
               Push_Object (A_Lambda);

               Pop_Error_Location;
            end;

         when Template_New_Expr =>
            if Top_Frame.Top_Context.An_Allocate_Callback /= null then
               Handle_New (Node.As_New_Expr.F_Tree);
            else
               Push_Match_False;
            end if;

            To_Match := False;

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

            if Node.As_Qualified_Match.F_Op = Template_Operator_Is then
               Top_Frame.Top_Context.Match_Mode := Match_Is;
            else
               Top_Frame.Top_Context.Match_Mode := Match_Has;
            end if;

            Evaluate_Expression (Node.As_Qualified_Match.F_Rhs);

            Pop_Frame_Context;

            To_Match := False;

         when others =>
            Error ("unexpected expression node kind: '" & Node.Kind'Wide_Wide_Image & "'");
      end case;


      if To_Match
        and then Top_Frame.Top_Context.Match_Mode not in Match_None | Match_Has
      then
         --  If we're matching, and we're not forcing the "has" mode, then check
         --  that the initial object we had on the stack matches the new one.

         if not Top_Frame.Top_Context.Matching_Object.Match_With_Top_Object then
            Pop_Object;
            Push_Match_False;
         end if;
      end if;

      Pop_Error_Location;
   end Evaluate_Expression;

   ----------------------------
   -- Analyze_Replace_String --
   ----------------------------

   Expression_Unit_Number : Integer := 1;

   procedure Analyze_Replace_String
     (Node : Template_Node'Class;
      On_Group : access procedure (Index : Integer; Value : W_Object) := null;
      On_Expression : access procedure (Expression : Template_Node) := null)
   is
      Str : constant Text_Type := Remove_Quotes (Node.Text);
      Context : constant Analysis_Context := Node.Unit.Context;

      Result : W_Text_Vector := new W_Text_Vector_Type;
      New_Text : W_String;

      Next_Index : Integer := Str'First;

      Current : Integer := Str'First;

      procedure Append_Text (Text : Text_Type) is
      begin
         New_Text := new W_String_Type;
         New_Text.Value := To_Unbounded_Text (Text);

         Result.A_Vector.Append (W_Object (New_Text));
      end Append_Text;

      procedure On_Error
        (Message : Text_Type;
         Filename : String;
         Loc : Source_Location)
      is
      begin
         Push_Error_Location
           (Node.Unit.Get_Filename,
            (Node.Sloc_Range.Start_Line, Node.Sloc_Range.Start_Column));

         Put_Line
           (To_Text (Get_Sloc_Str)
            & ": " & Message);

         raise Wrapping_Error;
      end On_Error;

      Prev_Error : Error_Callback_Type;
   begin
      Prev_Error := Error_Callback;
      Error_Callback := On_Error'Unrestricted_Access;

      if Str = "" then
         Append_Text ("");
         Error_Callback := Prev_Error;
         Push_Object (Result);

         return;
      end if;

      --  When evaluating a string, e.g.: "str \e<r.f_name> str", the expressions
      --  are not matched against the outside object. Every expression is also
      --  to be considered a root selection.

      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode := Match_None;
      Top_Frame.Top_Context.Is_Root_Selection := True;

      while Current <= Str'Last loop
         if Str (Current) = '\' then
            if Current /= Str'First then
               Append_Text (Str (Next_Index .. Current - 1));
            end if;

            Current := Current + 1;

            if Str (Current) = 'e' then
               Current := Current + 1;

               if Str (Current) = '<' then
                  Next_Index := Current;

                  while Next_Index < Str'Last and then Str (Next_Index) /= '>' loop
                     Next_Index := Next_Index + 1;
                  end loop;

                  Expression_Unit_Number := Expression_Unit_Number + 1;

                  declare
                     Expression_Unit : Analysis_Unit :=
                       Get_From_Buffer
                         (Context  => Context,
                          Filename => "internal expression" & Expression_Unit_Number'Img,
                          Buffer   => To_String (Str (Current + 1 .. Next_Index - 1)),
                          Rule     => Expression_Rule);
                  begin
                     if Has_Diagnostics (Expression_Unit) then
                        Error (To_Text (Diagnostics (Expression_Unit)(1).Message));
                     end if;

                     if On_Expression /= null then
                        On_Expression.All (Expression_Unit.Root);
                     else
                        Evaluate_Expression (Expression_Unit.Root);
                        Result.A_Vector.Append (W_Object (Pop_Object));
                     end if;
                  end;

                  Current := Next_Index + 1;
                  Next_Index := Current;
               else
                  Append_Text (Str (Current - 1 .. Current));
                  Next_Index := Current;
                  Current := Current + 1;
               end if;
            elsif Str (Current) = 'n' then
               Append_Text (To_Text (String'(1 => ASCII.LF)));
               Current := Current + 1;
               Next_Index := Current;
            elsif Str (Current) in '0' .. '9' then
               Next_Index := Current;

               while Next_Index < Str'Last and then Str (Next_Index) in '0' .. '9' loop
                  Next_Index := Next_Index + 1;
               end loop;

               if Str (Next_Index) not in '0' .. '9' then
                  Next_Index := Next_Index - 1;
               end if;

               declare
                  Group : Natural :=
                    Natural'Wide_Wide_Value (Str (Current .. Next_Index));
                  Value : W_Object := Top_Frame.Matched_Groups.Element (Group);
               begin
                  if On_Group /= null then
                     On_Group.all (Group, Value);
                  else
                     Append_Text (Value.To_String);
                  end if;
               end;

               Current := Next_Index + 1;
               Next_Index := Current;
            elsif Str (Current) = '\' then
               Append_Text ("\");
               Next_Index := Current;
               Current := Current + 1;
            else
               Current := Current + 1;
            end if;
         else
            Current := Current + 1;
         end if;
      end loop;

      if Next_Index <= Str'Last then
         -- Add the end of the text to the result

         Append_Text (Str (Next_Index .. Str'Last));
      end if;

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
         Push_Object
           (W_Object'
              (new W_Function_Type'
                   (Prefix => null, Call => Call_Convert_To_Text'Access)));

         return True;
      elsif Name = "string" then
         --  We're on an object to string conversion. Set the text object.
         --  When running the call, the actual text value will be computed and
         --  put in the object.
         --Push_Object (W_Object'(new W_String_Type));

         Error ("not implemented"); -- How about a kind in text_conversion?

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
     (Args : Argument_List;
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

      Parameter_Index := 1;

      for Param of Args loop
         if not Param.As_Argument.F_Name.Is_Null then
            In_Named_Section := True;
            Name_Node := Param.As_Argument.F_Name;
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;

            Name_Node := Name_For_Position.all (Parameter_Index);
         end if;

         if Perpare_Param_Evaluation /= null then
            Perpare_Param_Evaluation (Name_Node, Parameter_Index);
         end if;

         Evaluate_Expression (Param.F_Value);

         Parameter_Value := Pop_Object;

         Store_Param_Value (Name_Node, Parameter_Value);

         Parameter_Index := Parameter_Index + 1;
      end loop;

      Pop_Frame_Context;
   end Handle_Call_Parameters;

   procedure Handle_Template_Call
     (A_Template_Instance : W_Template_Instance;
      Args : Argument_List)
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
      begin
         A_Var := T_Var (A_Template_Instance.Defining_Entity.Get_Component (Name_Node.Text));

         if A_Template_Instance.Symbols.Contains (Name_Node.Text) then
            Ref := A_Template_Instance.Symbols.Element (Name_Node.Text);
         else
            Ref := new W_Reference_Type;
            A_Template_Instance.Symbols.Insert
              (Name_Node.Text, Ref);
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
      Args : Argument_List;
      Apply_To_All : Boolean)
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
      Traverse_Result : W_Object;
   begin
      --  Increment the visitor counter and set the current visitor id, as to
      --  track entities that are being visited by this iteration.
      Prev_Visit_Id := Current_Visitor_Id;
      Visitor_Counter := Visitor_Counter + 1;
      Current_Visitor_Id := Visitor_Counter;

      -- The analysis needs to be done within the previous frame (in particular
      -- to get capture groups) then all valuated symbols needs to be allocated
      -- to the next frame (in particular to be destroyed when popping).

      Handle_Call_Parameters
        (Args,
         Name_For_Position'Access,
         Store_Param_Value'Access);

      Push_Frame (A_Visitor);

      Top_Frame.Symbols.Move (Symbols);

      -- Apply_Wrapping_Program will push its own frame, which is local to the
      -- actual entity to be analyzed. The one pushed above is global to all
      -- calls and contains parameter evaluation, to be done only once.

      if not Apply_To_All then
         Apply_Wrapping_Program
           (An_Entity,
            A_Visitor,
            A_Visit_Action);
      else
         A_Visit_Action := An_Entity.Traverse
           (Child_Depth,
            True,
            Traverse_Result,
            Sub_Visitor'Access);
      end if;

      if A_Visit_Action in Over | Stop then
         Error ("visit action from visitor to caller not implemented");
      end if;

      --  Reset the visitor id to the previous value, we're back to the
      --  enclosing visitor invocation.
      Current_Visitor_Id := Prev_Visit_Id;

      Pop_Frame;
   end Handle_Visitor_Call;

   procedure Handle_Call (Node : Call_Expr'Class) is
      Called : W_Object;
      Result : W_Object;
   begin
      Push_Frame_Context;

      --  If we're matching, currently under the default ref mode, then move
      --  to the default call mode.

      if Top_Frame.Top_Context.Match_Mode = Match_Ref_Default then
         Top_Frame.Top_Context.Match_Mode := Match_Call_Default;
      end if;

      Evaluate_Expression (Node.F_Called);

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

      if Called.all in W_Call_To_Global_Type'Class then
         W_Call_To_Global (Called).Analyze_Parameters (Node.F_Args);

         --  When analyzing a global call, we're only analyzing the
         --  parameters. Push the call back to the stack for later consumption.
         Push_Object (Called);
      else
         Called.Push_Call_Result (Node.F_Args);
      end if;

      Result := Pop_Object;
      Pop_Object; -- Pop call symbol
      Push_Object (Result);

      Pop_Frame_Context;
   end Handle_Call;

   procedure Handle_Fold
     (Folded_Expression : Template_Node'Class; Node : Fold_Expr'Class)
   is
      Init_Value : W_Object;
   begin
      --  While it is not strictly mandatory (e.g. you can use fold to create
      --  a bunch of elements without caring of the aggregated result), fold
      --  commands usually have an accumulator. They usually look of the form:
      --     f: fold (expression, init, combine).
      --  in this case, the accumulator needs to receive the value of init
      --  and combine.

      Push_Frame_Context;
      Top_Frame.Top_Context.Is_Folding_Context := True;
      Top_Frame.Top_Context.Folding_Expression := Template_Node (Node.F_Combine);
      Top_Frame.Top_Context.Match_Mode := Match_None;
      Top_Frame.Top_Context.Is_Root_Selection := False;

      --  Inside the folded expression, we need to go back to a situation where
      --  self is top of the stack, as name can refer to the implicit self. Re
      --  push this value

      Push_Implicit_Self (Get_Implicit_Self);

      Evaluate_Expression (Node.F_Default);
      Init_Value := Pop_Object;

      if Top_Frame.Top_Context.Name_Captured /= "" then
         Top_Frame.Symbols.Include
           (To_Text (Top_Frame.Top_Context.Name_Captured), Init_Value);
      end if;

       --  Then pop both the self implicit value

      Pop_Object;

      Evaluate_Expression (Folded_Expression);

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
   end Handle_Fold;

   procedure Handle_New (Node : Create_Template_Tree'Class) is

      function Handle_Create_Template
        (A_Call : Template_Call'Class;
         Captured : Template_Node'Class)
         return W_Template_Instance
      is
         An_Object : W_Object;
         A_Template : T_Entity;
         A_Template_Instance : W_Template_Instance;
      begin
         An_Object := Evaluate_Expression (A_Call.F_Name);

         if An_Object.all not in W_Static_Entity_Type'Class then
            Error ("expected template reference");
         end if;

         A_Template := W_Static_Entity (An_Object).An_Entity;

         if A_Template.all not in T_Template_Type'Class then
            Error ("expected template reference");
         end if;

         A_Template_Instance := Create_Template_Instance (null, T_Template (A_Template));

         if not Captured.Is_Null then
            Top_Frame.Symbols.Include
              (Captured.Text, W_Object (A_Template_Instance));
         end if;

         Push_Implicit_New (A_Template_Instance);
         Handle_Template_Call (A_Template_Instance, A_Call.F_Args);
         Pop_Object;

         return A_Template_Instance;
      end Handle_Create_Template;

      function Handle_Create_Tree
        (A_Tree : Create_Template_Tree'Class; Parent : W_Template_Instance) return W_Template_Instance
      is
         Main_Node : W_Template_Instance;
         Dummy : W_Template_Instance;
      begin
         if not A_Tree.F_Root.Is_Null then
            Main_Node := Handle_Create_Template (A_Tree.F_Root, A_Tree.F_Captured);

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

            if not A_Tree.F_Tree.Is_Null then
               for C of A_Tree.F_Tree loop
                  Dummy := Handle_Create_Tree (C, Main_Node);
               end loop;
            end if;

            return Main_Node;
         else
            --  In the case of new ([T(), T()], every first level will need to
            --  be passed to the above level, and the last one will be the
            --  result of the operator.

            if not A_Tree.F_Tree.Is_Null then
               for C of A_Tree.F_Tree loop
                  Main_Node := Handle_Create_Tree (C, Parent);
               end loop;
            end if;

            return Main_Node;
         end if;
      end Handle_Create_Tree;

   begin
      Push_Frame_Context;
      Top_Frame.Top_Context.Match_Mode := Match_None;

      Push_Allocated_Entity (Handle_Create_Tree (Node, null));

      Pop_Frame_Context;
   end Handle_New;

   procedure Build_Lambda (A_Lambda : W_Lambda; Lambda_Expression : Template_Node) is
      Local_Symbols : Text_Sets.Set;

      function Not_Capture_Identifiers
        (Node : Template_Node'Class) return Libtemplatelang.Common.Visit_Status;

      function Capture_Identifiers
        (Node : Template_Node'Class) return Libtemplatelang.Common.Visit_Status;

      procedure Capture (Name : Text_Type) is
      begin
         if Local_Symbols.Contains (Name) then
            --  If the symbol is local to the lambda, then there's nothing to
            --  capture.
            return;
         end if;

         if Push_Global_Identifier (Name) then
            --  We found a global identifier to record. If not, we expect it
            --  to be resolved later when running the lambda.

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
      end Capture;

      procedure Capture_Group (Index : Integer; Value : W_Object) is
      begin
         Error ("not yet implemented");
      end Capture_Group;

      procedure Capture_Expression (Expression : Template_Node) is
      begin
         Expression.Traverse (Capture_Identifiers'Access);
      end Capture_Expression;

      function Capture_Identifiers
        (Node : Template_Node'Class) return Libtemplatelang.Common.Visit_Status
      is
      begin
         Push_Error_Location (Node);

         case Node.Kind is
            when Template_Selector =>
               Node.As_Selector.F_Left.Traverse (Capture_Identifiers'Access);

               Push_Frame_Context;
               Top_Frame.Top_Context.Is_Root_Selection := False;

               Node.As_Selector.F_Right.Traverse (Not_Capture_Identifiers'Access);

               Pop_Frame_Context;

               Pop_Error_Location;
               return Over;

            when Template_Token_Identifier | Template_Identifier =>
               Capture (Node.Text);

               Pop_Error_Location;
               return Over;

            when others =>
               Pop_Error_Location;

               return Not_Capture_Identifiers (Node);
         end case;
      end Capture_Identifiers;

      function Not_Capture_Identifiers
        (Node : Template_Node'Class) return Libtemplatelang.Common.Visit_Status is
      begin
         Push_Error_Location (Node);

         case Node.Kind is
            when Template_Match_Capture =>
               declare
                  Name : Text_Type := Node.As_Match_Capture.F_Captured.Text;
               begin
                  --  If the name isn't already identified as a local name,
                  --  identify it as such for the remainder of the analysis.
                  --  Otherwise, just pass through.

                  if not Local_Symbols.Contains (Name) then
                     Local_Symbols.Insert (Name);
                     Node.As_Match_Capture.F_Expression.Traverse
                       (Capture_Identifiers'Access);
                     Local_Symbols.Delete (Name);
                     return Over;
                  else
                     return Into;
                  end if;
               end;
            when Template_Str =>
               --  Analyze_Replace_String ABI is expecting that capturing an
               --  expression pushes a value on the stack (it's going to get
               --  popped. So use Capture_Expression_And_Push_Dummy in order
               --  to avoid popping the top of the stack.

               Analyze_Replace_String
                 (Node,
                  Capture_Group'Access,
                  Capture_Expression'Access);

               Pop_Object;

               Pop_Error_Location;
               return Over;
            when Template_Call_Expr | Template_New_Expr =>
               --  Resolved the called identifier

               Push_Frame_Context;
               Top_Frame.Top_Context.Is_Root_Selection := False;

               for Arg of Node.As_Call_Expr.F_Args loop
                  Arg.Traverse (Capture_Identifiers'Access);
               end loop;

               Pop_Frame_Context;

               Pop_Error_Location;
               return Over;

         when others =>

            Pop_Error_Location;
            return Into;
         end case;
      end Not_Capture_Identifiers;
   begin
      Capture_Expression (Lambda_Expression);
      A_Lambda.Expression := Lambda_Expression;
      A_Lambda.Implicit_Self := W_Node (Get_Implicit_Self);
      A_Lambda.Implicit_New := W_Node (Get_Implicit_New);
      A_Lambda.Lexical_Scope := Top_Frame.Lexical_Scope;
   end Build_Lambda;

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

      Evaluate_Expression (A_Lambda.Expression);
      Result := Pop_Object;
      Pop_Frame;
      Push_Object (Result);
   end Run_Lambda;

end Wrapping.Runtime.Analysis;
