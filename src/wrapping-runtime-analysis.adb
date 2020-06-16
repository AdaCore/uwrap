with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings; use Ada.Strings;
with GNAT.Regpat; use GNAT.Regpat;

with Langkit_Support.Diagnostics;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libtemplatelang.Common;

with Wrapping.Regex; use Wrapping.Regex;
with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Utils; use Wrapping.Utils;
with Wrapping.Runtime.Functions; use Wrapping.Runtime.Functions;

package body Wrapping.Runtime.Analysis is

   procedure Handle_Identifier (Node : Libtemplatelang.Analysis.Template_Node'Class);
   procedure Handle_Call (Node : Libtemplatelang.Analysis.Template_Node'Class);
   procedure Handle_Template_Call (A_Template_Instance : Template_Instance; Args : Libtemplatelang.Analysis.Argument_List);
   procedure Handle_Visitor_Call
     (An_Entity : Language_Entity;
      A_Visitor : Semantic.Structure.Visitor;
      Args : Libtemplatelang.Analysis.Argument_List;
      Apply_To_All : Boolean);
   function Analyze_Visitor (E : access Language_Entity_Type'Class) return Visit_Action;

   procedure Analyze_Replace_String
     (Node : Libtemplatelang.Analysis.Template_Node'Class;
      On_Group : access procedure (Index : Integer; Value : Runtime_Object) := null;
      On_Expression : access procedure (Expression : Libtemplatelang.Analysis.Template_Node) := null);

   function Visit_Expression (Node : Libtemplatelang.Analysis.Template_Node'Class) return Libtemplatelang.Common.Visit_Status;
   procedure Build_Lambda (A_Lambda : Runtime_Lambda; Lambda_Expression : Libtemplatelang.Analysis.Template_Node);

   procedure Push_Error_Location
     (An_Entity : access Semantic.Structure.Entity_Type'Class)
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

   procedure Push_Entity (An_Entity : access Language_Entity_Type'Class) is
   begin
      Top_Frame.Data_Stack.Append (new Runtime_Language_Entity_Type'(Value => Language_Entity (An_Entity), others => <>));
   end Push_Entity;

   procedure Push_Object (An_Object : access Runtime_Object_Type'Class) is
   begin
      Top_Frame.Data_Stack.Append (Runtime_Object (An_Object));
   end Push_Object;

   procedure Push_Implicit_Self (An_Entity : access Language_Entity_Type'Class) is
   begin
      Top_Frame.Data_Stack.Append
        (new Runtime_Language_Entity_Type'
           (Value => Language_Entity (An_Entity),
            Is_Implicit_Self => True,
            others => <>));
   end Push_Implicit_Self;

   procedure Push_Implicit_New (An_Entity : access Language_Entity_Type'Class) is
   begin
      Top_Frame.Data_Stack.Append
        (new Runtime_Language_Entity_Type'
           (Value => Language_Entity (An_Entity),
            Is_Implicit_New => True,
            others => <>));
   end Push_Implicit_New;

   procedure Push_Allocated_Entity (An_Entity : access Language_Entity_Type'Class) is
   begin
      Top_Frame.Data_Stack.Append
        (new Runtime_Language_Entity_Type'
           (Value => Language_Entity (An_Entity),
            Is_Allocated => True,
            others => <>));
   end Push_Allocated_Entity;

   procedure Push_Temporary_Name (Name : Text_Type; Counter : in out Integer) is
   begin
      if Top_Frame.Temp_Names.Contains (Name) then
         Top_Frame.Data_Stack.Append
           (new Runtime_Text_Type'
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
              (new Runtime_Text_Type'(Value => To_Unbounded_Text (Tmp)));
         end;
      end if;
   end Push_Temporary_Name;

   procedure Pop_Object (Number : Positive := 1) is
   begin
      Top_Frame.Data_Stack.Delete_Last (Count_Type (Number));
   end Pop_Object;

   function Pop_Object return Runtime_Object is
      Result : Runtime_Object;
   begin
      Result := Top_Frame.Data_Stack.Last_Element;
      Pop_Object;
      return Result;
   end Pop_Object;

   procedure Push_Frame (Lexical_Scope : access Semantic.Structure.Entity_Type'Class) is
      New_Frame : Data_Frame := new Data_Frame_Type;
   begin
      New_Frame.Parent_Frame := Top_Frame;
      New_Frame.Lexical_Scope := Semantic.Structure.Entity (Lexical_Scope);

      if Top_Frame /= null then
         New_Frame.An_Allocate_Callback := Top_Frame.An_Allocate_Callback;
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

   function Get_Implicit_Self return Language_Entity is
   begin
      for I in reverse Top_Frame.Data_Stack.First_Index .. Top_Frame.Data_Stack.Last_Index loop
         if Top_Frame.Data_Stack.Element (I).all in Runtime_Language_Entity_Type
           and then Runtime_Language_Entity (Top_Frame.Data_Stack.Element (I)).Is_Implicit_Self
         then
            return Runtime_Language_Entity (Top_Frame.Data_Stack.Element (I)).Value;
         end if;
      end loop;

      return null;
   end Get_Implicit_Self;

   function Get_Implicit_New return Language_Entity is
   begin
      for I in reverse Top_Frame.Data_Stack.First_Index .. Top_Frame.Data_Stack.Last_Index loop
         if Top_Frame.Data_Stack.Element (I).all in Runtime_Language_Entity_Type
           and then Runtime_Language_Entity (Top_Frame.Data_Stack.Element (I)).Is_Implicit_New
         then
            return Runtime_Language_Entity (Top_Frame.Data_Stack.Element (I)).Value;
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
              (new Runtime_Text_Type'
                 (Value => To_Unbounded_Text (Matched_Text)));

            if Name /= "" then
               Top_Frame.Symbols.Include
                 (Name,
                  new Runtime_Text_Type'
                    (Value => To_Unbounded_Text (Matched_Text)));
            end if;
         end;
      end loop;

      return True;
   end Match;

   procedure Apply_Wrapping_Program
     (A_Language_Entity : Language_Entity;
      Lexical_Scope     : access Semantic.Structure.Entity_Type'Class;
      A_Visit_Action    : in out Visit_Action)
   is
      Self_Element : Runtime_Language_Entity := new Runtime_Language_Entity_Type; -- TODO: We may not need to instantiate here

      procedure Allocate (E : access Language_Entity_Type'Class) is
      begin
         --  when allocating an object outside of a browsing function, nothign
         --  special to do
         null;
      end Allocate;

      procedure Create_And_Set_Template_Instance
        (A_Command  : Command;
         Expression : Libtemplatelang.Analysis.Template_Node'Class);
      --  This will create a template instance from the clause information,
      --  setting parameter expression and adding it to the relevant language
      --  object.

      procedure Create_And_Set_Template_Instance
        (A_Command : Command;
         Expression : Libtemplatelang.Analysis.Template_Node'Class)
      is
      begin
         Evaluate_Expression (Expression);
      end Create_And_Set_Template_Instance;

      procedure Apply_Template_Action
        (A_Language_Entity : Language_Entity; Template_Clause : Weave_Or_Wrap)
      is
         A_Template_Instance : Template_Instance;
         Self_Weave : Boolean := False;
         Result : Runtime_Object;
      begin
         Push_Error_Location (Template_Clause.Node);

         if Template_Clause.Is_Null then
            --  We've set a null template - the objective is to prevent this
            --  entity to be wrapped by this template.

            if Template_Clause.Arguments.Children_Count /= 1 then
               Error ("expected one argument to null");
            end if;

            Evaluate_Expression (Template_Clause.Arguments.Child (1).As_Argument.F_Value);
            Result := Pop_Object;

            if Result.all not in Runtime_Static_Entity_Type
              or else Runtime_Static_Entity (Result).An_Entity.all
                not in Semantic.Structure.Template_Type'Class
            then
               Error ("expected template reference");
            end if;

            A_Language_Entity.Forbidden_Template_Names.Include
              (Semantic.Structure.Template
                 (Runtime_Static_Entity (Result).An_Entity).Full_Name);

            --  TODO: remove the template if it's already been created in the
            --  context of a weave clause
         else
            if Template_Clause.Call_Reference = null then
               --  No name to the call, that means that we're expecting to self-weave
               --  the current template.
               if Template_Clause.all not in Weave_Type'Class then
                  Error ("self wrap not allowed, either weave or provide a template or visitor name");
               elsif A_Language_Entity.all in Template_Instance_Type'Class then
                  A_Template_Instance := Template_Instance (A_Language_Entity);
               else
                  Error ("only template instances can be self weaved");
               end if;

               Self_Weave := True;
            elsif Template_Clause.Call_Reference.all in Template_Type'Class then
               A_Template_Instance := A_Language_Entity.Get_Template_Instance
                 (Semantic.Structure.Template (Template_Clause.Call_Reference));

               if (Template_Clause.all in Weave_Type'Class
                   or else A_Template_Instance = null
                   or else A_Template_Instance.Is_Wrapping = False)
                 and then not A_Language_Entity.Forbidden_Template_Names.Contains
                      (Template_Clause.Call_Reference.Full_Name)
               then
                  if A_Template_Instance = null then
                     A_Template_Instance := A_Language_Entity.Create_Template_Instance
                       (Semantic.Structure.Template (Template_Clause.Call_Reference));
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

      procedure Apply_Command (A_Command : Command) is
         Matched : Boolean;
         Match_Result : Runtime_Object;
      begin
         --  The command is the enclosing scope for all of its clauses. It
         --  will in particular receive the matching groups and the temporary
         --  values that can be used consistently in the various clauses
         Push_Frame (A_Command);
         Push_Implicit_Self (A_Language_Entity);

         Top_Frame.An_Allocate_Callback := Allocate'Unrestricted_Access;

         if not A_Command.Match_Expression.Is_Null then
            Top_Frame.Context := Match_Context;
            Evaluate_Expression (A_Command.Match_Expression);
            Top_Frame.Context := Generic_Context;

            Match_Result := Pop_Object;

            Matched := Match_Result /= Match_False;
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
                     Entity_Target : Language_Entity;
                  begin
                     if not A_Command.Template_Clause.Target_Object.Is_Null then
                        Evaluate_Expression (A_Command.Template_Clause.Target_Object);
                        Entity_Target := Runtime_Language_Entity (Pop_Object).Value;

                        --  A number of the function below assume that
                        -- children for the entity are set - make sure that
                        -- they are
                        Language_Entity_Type'Class (Entity_Target.all).Pre_Visit;
                     else
                        Entity_Target := A_Language_Entity;
                     end if;

                     if A_Command.Template_Clause.Call_Reference /= null
                       or else not A_Command.Template_Clause.Arguments.Is_Null
                     then
                        -- There is an explicit template call. Pass this on either the
                        -- current template or the whole tree

                        if A_Command.Template_Clause.Call_Reference/= null
                          and then A_Command.Template_Clause.Call_Reference.all in Visitor_Type'Class
                        then
                           Handle_Visitor_Call
                             (Entity_Target,
                              Semantic.Structure.Visitor (A_Command.Template_Clause.Call_Reference),
                              A_Command.Template_Clause.Arguments,
                              A_Command.Template_Clause.Is_All);
                        elsif A_Command.Template_Clause.Is_All then
                           -- TODO: This could all be moved to Handle Template Call instead, and
                           -- would be consistent with the fact that everything above is
                           -- in Handle_Visitor_Call
                           declare
                              function Apply_Template (E : access Language_Entity_Type'Class) return Visit_Action
                              is
                              begin
                                 Apply_Template_Action (Language_Entity (E), A_Command.Template_Clause);

                                 return Into;
                              end Apply_Template;
                           begin
                              A_Visit_Action := Entity_Target.Traverse
                                (A_Mode       => Child_Depth,
                                 Include_Self => True,
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
                                   (Child_Depth, True, Analyze_Visitor'Access);

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

                  A_Command.Traverse_Expression.Traverse (Visit_Expression'Access);

                  if Top_Frame.Data_Stack.Last_Element.all in Runtime_Traverse_Decision_Type'Class then
                     --  In this case, a decision was taken as to the continuation of the
                     --  iteration.

                     declare
                        A_Decision : Runtime_Traverse_Decision := Runtime_Traverse_Decision
                          (Top_Frame.Data_Stack.Last_Element);
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
               if A_Command.Else_Actions.all in Command_Type'Class then
                  Apply_Command (Command (A_Command.Else_Actions));
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
         if Wrapping_Entity.all in Command_Type then
            Apply_Command (Command (Wrapping_Entity));
         elsif Wrapping_Entity.all in Module_Type'Class
           or else Wrapping_Entity.all in Namespace_Type'Class
         then
            Apply_Wrapping_Program
              (A_Language_Entity, Wrapping_Entity, A_Visit_Action);
         end if;
      end loop;

      Pop_Frame;
   end Apply_Wrapping_Program;

   function Analyze_Visitor (E : access Language_Entity_Type'Class) return Visit_Action is
      A_Visit_Action : Visit_Action := Into;
   begin
      --  Check if this entity has already been analyzed through this visitor
      --  invocation.

      --  First, pop any id in the entity that may be greater than the current
      --  one. If they are greater and we're on a lower one, it means that they
      --  are over.

      while E.Visited_Stack.Length > 0
        and then E.Visited_Stack.Last_Element > Current_Visitor_Id
      loop
         E.Visited_Stack.Delete_Last;
      end loop;

      if E.Visited_Stack.Length > 0
        and then E.Visited_Stack.Last_Element = Current_Visitor_Id
      then
         --  If the last id in the stack is the current one, they we've already
         --  visited this entity. We already also made the decisions on sub
         --  entities. Stop the iteration.

         return Over;
      else
         --  Otherwise, stack this visitor Id and visit. We need not to remove
         --  this id at the end, as to make sure that potential iterations
         --  on this visit don't cover this node again

         E.Visited_Stack.Append (Current_Visitor_Id);

         Apply_Wrapping_Program
           (Language_Entity (E),
            Wrapping.Semantic.Analysis.Root,
            A_Visit_Action);

         return A_Visit_Action;
      end if;
   end Analyze_Visitor;

   procedure Analyse (Root_Entity : Language_Entity) is
      File_Template : Wrapping.Semantic.Structure.Template;
      Out_Template : Wrapping.Semantic.Structure.Template;
      A_Template_Instance : Template_Instance;
      Dummy_Action : Visit_Action;
   begin
      -- Set the visitor id - we're on the main iteration, id is 0.

      Current_Visitor_Id := 0;

      Dummy_Action := Root_Entity.Traverse
        (Child_Depth, True, Analyze_Visitor'Access);

      File_Template := Wrapping.Semantic.Structure.Template
        (Resolve_Module_By_Name ("standard").
             Children_Indexed.Element ("file"));

      Out_Template := Wrapping.Semantic.Structure.Template
        (Resolve_Module_By_Name ("standard").
             Children_Indexed.Element ("out"));

      while Templates_To_Traverse.Length > 0 loop
         declare
            Next_Iteration : Template_Instance_Vectors.Vector;
         begin
            Next_Iteration.Move (Templates_To_Traverse);
            Templates_To_Traverse.Clear;

            -- Reset the visitor id - we're on the main iteration, id is 0.

            Current_Visitor_Id := 0;

            for T of Next_Iteration loop
               Dummy_Action := T.Traverse
                 (Child_Depth, True, Analyze_Visitor'Access);
            end loop;

            for Created_Template of Next_Iteration loop
               A_Template_Instance := Template_Instance (Created_Template);

               if Instance_Of (A_Template_Instance.Template, File_Template) then
                  declare
                     Path_Object : Runtime_Object;
                     Content_Object : Runtime_Object;
                     Output_File : File_Type;
                  begin
                     Push_Frame (Root);

                     if not A_Template_Instance.Push_Value ("path") then
                        Error ("'path' component not found in file template");
                     end if;

                     Path_Object := Pop_Object;

                     if not A_Template_Instance.Push_Value ("content") then
                        Error ("'content' component not found in file template");
                     end if;

                     Content_Object := Pop_Object;

                     Create
                       (Output_File,
                        Out_File,
                        To_String (Path_Object.To_Text));

                     Put (Output_File, Content_Object.To_Text);

                     Close (Output_File);

                     Pop_Frame;
                  end;
               elsif Instance_Of (A_Template_Instance.Template, Out_Template) then
                  declare
                     Content_Object : Runtime_Object;
                  begin
                     Push_Frame (Root);

                     if not A_Template_Instance.Push_Value ("content") then
                        Error ("'content' component not found in file template");
                     end if;

                     Content_Object := Pop_Object;

                     Put (Content_Object.To_Text);

                     Pop_Frame;
                  end;
               end if;
            end loop;
         end;
      end loop;
   end Analyse;

   procedure Evaluate_Expression
     (Node : Libtemplatelang.Analysis.Template_Node'Class)
   is
      -- TODO: Review the need for this confusing function that doesn't really traverse
      Dummy : Libtemplatelang.Common.Visit_Status := Visit_Expression (Node);
   begin
      null;
   end Evaluate_Expression;

   function Visit_Expression (Node : Libtemplatelang.Analysis.Template_Node'Class) return Libtemplatelang.Common.Visit_Status is
      use Libtemplatelang.Analysis;
      use Libtemplatelang.Common;
   begin
      Push_Error_Location (Node);

      case Node.Kind is
         when Template_Match_Capture =>
            --  This expression captures the result of the underlying
            --  expression and lets its value pass through.

            Node.As_Match_Capture.F_Expression.Traverse (Visit_Expression'Access);

            if Top_Frame.Data_Stack.Last_Element /= Match_False then
               Top_Frame.Symbols.Include
                 (Node.As_Match_Capture.F_Captured.Text,
                  Top_Frame.Data_Stack.Last_Element);
            end if;

            Pop_Error_Location;
            return Over;

         when Template_Selector =>
            declare
               Result : Runtime_Object;
            begin
               --  In a selector, we compute the left object, build the right
               --  expression based on the left object, and then put the result
               --  on the left object on the stack.

               if not Node.As_Selector.F_Left.Is_Null then
                  Node.As_Selector.F_Left.Traverse (Visit_Expression'Access);
                  Node.As_Selector.F_Right.Traverse (Visit_Expression'Access);
                  Result := Pop_Object;
                  Pop_Object;
                  Push_Object (Result);
               else
                  Node.As_Selector.F_Right.Traverse (Visit_Expression'Access);
               end if;
            end;

            Pop_Error_Location;
            return Over;
         when Template_Binary_Expr =>
            --  The convention for "and" and "or" binary operators is to push to
            --  the stack the last object that matched, otherwise false. This
            --  allows to capture that object later on, which can be useful for
            --  example if that object is a newly allocated one.

            declare
               Left, Right : Runtime_Object;
            begin
               Node.As_Binary_Expr.F_Lhs.Traverse (Visit_Expression'Access);
               Left := Pop_Object;

               if Node.As_Binary_Expr.F_Op.Kind = Template_Operator_And then
                  if Left /= Match_False then
                     Node.As_Binary_Expr.F_Rhs.Traverse (Visit_Expression'Access);
                     Right := Pop_Object;

                     if Right /= Match_False then
                        Push_Object (Right);
                     else
                        Push_Match_False;
                     end if;
                  else
                     Push_Match_False;
                  end if;
               elsif Node.As_Binary_Expr.F_Op.Kind = Template_Operator_Or then
                  if Left /= Match_False then
                     Push_Object (Left);
                  else
                     Node.As_Binary_Expr.F_Rhs.Traverse (Visit_Expression'Access);
                     Right := Pop_Object;

                     if Right /= Match_False then
                        Push_Object (Right);
                     else
                        Push_Match_False;
                     end if;
                  end if;
               end if;
            end;

            Pop_Error_Location;
            return Over;
         when Template_Unary_Expr =>
            declare
               Right : Runtime_Object;
            begin
               Node.As_Unary_Expr.F_Rhs.Traverse (Visit_Expression'Access);
               Right := Pop_Object;

               if Node.As_Unary_Expr.F_Op.Kind = Template_Operator_Not then
                  if Right = Match_False then
                     Push_Match_True (Top_Frame.Data_Stack.Last_Element);
                  else
                     Push_Match_False;
                  end if;
               end if;
            end;

            Pop_Error_Location;
            return Over;

         when Template_Literal =>
            if Node.Text = "true" then
               Push_Match_True (Top_Frame.Data_Stack.Last_Element);
            elsif Node.Text = "false" then
               Push_Match_False;
            else
               Error ("unkown literal '" & Node.Text & "'");
            end if;

            Pop_Error_Location;
            return Over;

         when Template_Token_Identifier | Template_Identifier =>
            Handle_Identifier (Node);

            Pop_Error_Location;
            return Over;
         when Template_Number =>
            declare
               Val : Runtime_Integer := new Runtime_Integer_Type;
            begin
               Val.Value := Integer'Wide_Wide_Value (Node.Text);

               Push_Object (Val);
            end;

            Pop_Error_Location;
            return Over;
         when Template_Str =>
            Analyze_Replace_String (Node);

            if Top_Frame.Context = Match_Context then
               declare
                  To_Match : Runtime_Object := Pop_Object;
               begin
                  if not Get_Implicit_Self.Push_Match_Result (To_Match, No_Argument_List) then
                     Push_Match_False;
                  end if;
               end;
            end if;

            Pop_Error_Location;
            return Over;
         when Template_Call_Expr =>
            --  Resolved the called identifier

            Handle_Call (Node);

            Pop_Error_Location;
            return Over;

         when Template_Lambda_Expr =>
            declare
               A_Lambda : Runtime_Lambda := new Runtime_Lambda_Type;
            begin
               Build_Lambda (A_Lambda, Node.As_Lambda_Expr.F_Expression);
               Push_Object (A_Lambda);

               Pop_Error_Location;
               return Over;
            end;

         when Template_New_Expr =>
            if Top_Frame.An_Allocate_Callback /= null then
               declare
                  An_Object : Runtime_Object;
                  A_Template : Semantic.Structure.Entity;
                  A_Template_Instance : Template_Instance;
                  Last_Context : Frame_Context := Top_Frame.Context;
               begin
                  Top_Frame.Context := Generic_Context;
                  Evaluate_Expression (Node.As_New_Expr.F_Name);
                  An_Object := Pop_Object;

                  if An_Object.all not in Runtime_Static_Entity_Type'Class then
                     Error ("expected template reference");
                  end if;

                  A_Template := Runtime_Static_Entity (An_Object).An_Entity;

                  if A_Template.all not in Semantic.Structure.Template_Type'Class then
                     Error ("expected template reference");
                  end if;

                  A_Template_Instance := Create_Template_Instance (null, Semantic.Structure.Template (A_Template));

                  Push_Implicit_New (A_Template_Instance);
                  Handle_Template_Call (A_Template_Instance, Node.As_New_Expr.F_Args);
                  Pop_Object;

                  Push_Allocated_Entity (A_Template_Instance);
                  Top_Frame.An_Allocate_Callback.all (A_Template_Instance);

                  Top_Frame.Context := Last_Context;
               end;
            else
               Push_Match_False;
            end if;

            Pop_Error_Location;
            return Over;

         when others =>
            Error ("unexpected expression node kind: '" & Node.Kind'Wide_Wide_Image & "'");
            Pop_Error_Location;
            return Into;
      end case;
   end Visit_Expression;

   ----------------------------
   -- Analyze_Replace_String --
   ----------------------------

   Expression_Unit_Number : Integer := 1;

   procedure Analyze_Replace_String
     (Node : Libtemplatelang.Analysis.Template_Node'Class;
      On_Group : access procedure (Index : Integer; Value : Runtime_Object) := null;
      On_Expression : access procedure (Expression : Libtemplatelang.Analysis.Template_Node) := null)
   is
      use Libtemplatelang.Analysis;
      use Libtemplatelang.Common;
      use Langkit_Support.Diagnostics;

      Str : constant Text_Type := Remove_Quotes (Node.Text);
      Context : constant Libtemplatelang.Analysis.Analysis_Context := Node.Unit.Context;

      Result : Runtime_Text_Container := new Runtime_Text_Container_Type;
      New_Text : Runtime_Text;

      Next_Index : Integer := Str'First;

      Current : Integer := Str'First;

      procedure Append_Text (Text : Text_Type) is
      begin
         New_Text := new Runtime_Text_Type;
         New_Text.Value := To_Unbounded_Text (Text);

         Result.Texts.Append (Runtime_Text_Expression (New_Text));
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

         Error (Message);
      end On_Error;

      Prev_Error : Error_Callback_Type;
   begin
      Prev_Error := Error_Callback;
      Error_Callback := On_Error'Unrestricted_Access;

      if Str = "" then
         Append_Text ("");
         Error_Callback := Prev_Error;
         return;
      end if;

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
                        Result.Texts.Append (Pop_Object.To_Text_Expression);
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
                  Value : Runtime_Object := Top_Frame.Matched_Groups.Element (Group);
               begin
                  if On_Group /= null then
                     On_Group.all (Group, Value);
                  else
                     Append_Text (Value.To_Text);
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
   end Analyze_Replace_String;

   function Push_Global_Identifier (Name : Text_Type) return Boolean is
      A_Module : Semantic.Structure.Module;
      Tentative_Symbol : Runtime_Object;
      A_Semantic_Entity : Semantic.Structure.Entity;

      Implicit_Self : Language_Entity;
      Implicit_New  : Language_Entity;
   begin
      Implicit_Self := Get_Implicit_Self;
      Implicit_New := Get_Implicit_New;

      if Name = "self" then
         Push_Entity (Implicit_Self);

         return True;
      elsif Name = "new" and then Implicit_New /= null then
         Push_Entity (Implicit_New);

         return True;
      end if;

      -- Check in the dynamic symols in the frame

      Tentative_Symbol := Get_Visible_Symbol (Top_Frame.all, Name);

      A_Module := Get_Module (Top_Frame.all);

      -- Check in the static symbols in the module

      if A_Module.Children_Indexed.Contains (Name) then
         if Tentative_Symbol = null then
            A_Semantic_Entity := A_Module.Children_Indexed (Name);

            if A_Semantic_Entity.all in Template_Type'Class then
               Tentative_Symbol := new Runtime_Static_Entity_Type'
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

               if A_Semantic_Entity.all in Template_Type'Class then
                  Tentative_Symbol := new Runtime_Static_Entity_Type'
                    (An_Entity => A_Semantic_Entity);
               end if;
            else
               Error ("can't reference " & Name & ", multiple definitions hiding");
            end if;
         end if;
      end loop;

      -- Check in the namesaces symbols

      if Root.Children_Indexed.Contains (Name) then
         if Tentative_Symbol = null then
            A_Semantic_Entity := Root.Children_Indexed.Element (Name);

            if A_Semantic_Entity.all in Namespace_Type'Class
              or else A_Semantic_Entity.all in Module_Type'Class
            then
               Tentative_Symbol := new Runtime_Static_Entity_Type'
                 (An_Entity => A_Semantic_Entity);
            end if;
         else
            Error ("can't reference " & Name & ", multiple definitions hiding");
         end if;
      end if;

      if Tentative_Symbol = null then
         if Top_Frame.Context = Match_Context then
            Push_Match_False;
         else
            return False;
         end if;
      else
         Push_Object (Tentative_Symbol);
      end if;

      return True;
   end Push_Global_Identifier;

   procedure Handle_Global_Identifier (Name : Text_Type) is
   begin
      if not Push_Global_Identifier (Name) then
         Error ("can't find global reference to '" & Name & "'");
      end if;
   end Handle_Global_Identifier;

   procedure Handle_Identifier (Node : Libtemplatelang.Analysis.Template_Node'Class) is
      Prefix_Entity : Language_Entity;
      A_Semantic_Entity : Semantic.Structure.Entity;

      procedure Handle_Static_Entity_Selection is
         An_Entity : Semantic.Structure.Entity;
      begin
         An_Entity := Runtime_Static_Entity (Top_Frame.Data_Stack.Last_Element).An_Entity;

         if not An_Entity.Children_Indexed.Contains (Node.Text) then
            Error ("'" & Node.Text & "' not found");
         end if;

         A_Semantic_Entity := An_Entity.Children_Indexed.Element (Node.Text);

         Push_Object
           (Runtime_Object'(new Runtime_Static_Entity_Type'(An_Entity => A_Semantic_Entity)));
      end Handle_Static_Entity_Selection;

      procedure Handle_Language_Entity_Selection is
         Name : Text_Type := Node.Text;

         Implicit_Self : Language_Entity;
         Implicit_New  : Language_Entity;

         Found_Self_Entity : Boolean;
         Found_New_Entity : Boolean;

         Self_Object : Runtime_Object;
      begin
         -- We're resolving a reference to an entity

         Prefix_Entity := Runtime_Language_Entity (Top_Frame.Data_Stack.Last_Element).Value;

         if Runtime_Language_Entity
           (Top_Frame.Data_Stack.Last_Element).Is_Implicit
         then
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

            Error ("'" & Node.Text & "' not found");
         else
            if Prefix_Entity.Push_Value (Name) then
               --  We found a component of the entity and it has been pushed
               return;
            else
               Error ("'" & Name & "' component not found");
            end if;
         end if;
      end Handle_Language_Entity_Selection;

   begin
      if Top_Frame.Data_Stack.Length /= 0 then
         if Top_Frame.Data_Stack.Last_Element.all in Runtime_Static_Entity_Type'Class then
            Handle_Static_Entity_Selection;
         elsif Top_Frame.Data_Stack.Last_Element.all in Runtime_Language_Entity_Type then
            Handle_Language_Entity_Selection;
         else
            Handle_Global_Identifier (Node.Text);
         end if;
      else
         Handle_Global_Identifier (Node.Text);
      end if;
   end Handle_Identifier;

   procedure Handle_Call_Parameters
     (Args : Libtemplatelang.Analysis.Argument_List;
      Name_For_Position : access function (Position : Integer) return Libtemplatelang.Analysis.Template_Node;
      Store_Param_Value : access procedure (Name_Node : Libtemplatelang.Analysis.Template_Node; Value : Runtime_Object))
   is
      Parameter_Index : Integer;
      Parameter_Value : Runtime_Object;
      In_Named_Section : Boolean := False;
      Name_Node : Libtemplatelang.Analysis.Template_Node;
   begin
      Parameter_Index := 1;

      for Param of Args loop
         Evaluate_Expression (Param.F_Value);

         Parameter_Value := Pop_Object;

         if not Param.As_Argument.F_Name.Is_Null then
            In_Named_Section := True;
            Name_Node := Param.As_Argument.F_Name;
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;

            Name_Node := Name_For_Position.all (Parameter_Index);
         end if;

         Store_Param_Value (Name_Node, Parameter_Value);

         Parameter_Index := Parameter_Index + 1;
      end loop;
   end Handle_Call_Parameters;

   procedure Handle_Template_Call
     (A_Template_Instance : Template_Instance;
      Args : Libtemplatelang.Analysis.Argument_List)
   is
      function Name_For_Position (Position : Integer) return Libtemplatelang.Analysis.Template_Node is
      begin
         return A_Template_Instance.Template.Get_Variable_For_Index
           (Position).Name_Node;
      end Name_For_Position;

      procedure Store_Param_Value (Name_Node : Libtemplatelang.Analysis.Template_Node; Value : Runtime_Object) is
         Container : Runtime_Text_Container;
      begin
         if A_Template_Instance.Symbols.Contains (Name_Node.Text) then
            Container := Runtime_Text_Container
              (A_Template_Instance.Symbols.Element (Name_Node.Text));
         else
            Container := new Runtime_Text_Container_Type;
            A_Template_Instance.Symbols.Insert
              (Name_Node.Text, Runtime_Object (Container));
         end if;

         Container.Texts.Append (Value.To_Text_Expression);
      end Store_Param_Value;
   begin
      Handle_Call_Parameters
        (Args,
         Name_For_Position'Access,
         Store_Param_Value'Access);
   end Handle_Template_Call;

   procedure Handle_Visitor_Call
     (An_Entity : Language_Entity;
      A_Visitor : Semantic.Structure.Visitor;
      Args : Libtemplatelang.Analysis.Argument_List;
      Apply_To_All : Boolean)
   is
      Symbols : Runtime_Object_Maps.Map;

      function Name_For_Position (Position : Integer) return Libtemplatelang.Analysis.Template_Node is
      begin
         return A_Visitor.Arguments_Ordered.Element (Position).Name_Node;
      end Name_For_Position;

      procedure Store_Param_Value (Name_Node : Libtemplatelang.Analysis.Template_Node; Value : Runtime_Object) is
      begin
         Symbols.Insert (Name_Node.Text, Value);
      end Store_Param_Value;

      function Sub_Visitor (E : access Language_Entity_Type'Class) return Visit_Action is
         A_Visit_Action : Visit_Action := Into;
      begin
         Apply_Wrapping_Program
           (Language_Entity (E),
            A_Visitor,
            A_Visit_Action);

         return A_Visit_Action;
      end Sub_Visitor;


      Prev_Visit_Id : Integer;
      A_Visit_Action : Visit_Action := Unknown;
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

   procedure Handle_Call (Node : Libtemplatelang.Analysis.Template_Node'Class) is
      use Libtemplatelang.Analysis;

      procedure Handle_Function_Call is
         A_Function : Runtime_Function_Reference :=
           Runtime_Function_Reference (Pop_Object);
      begin
         if not A_Function.Prefix.Push_Call_Result
           (To_Text (A_Function.Name), Node.As_Call_Expr.F_Args)
         then
            Error ("function '" & To_Text (A_Function.Name) & "' can't be called");
         end if;
      end Handle_Function_Call;

      procedure Handle_Match is
         Match_Object : Runtime_Object;
         Self : Language_Entity;
         Scope : Semantic.Structure.Entity;
         Selected : Semantic.Structure.Entity;
         Called : Runtime_Object;
         Result : Runtime_Object;
         Match_Is_Implicit : Boolean;
      begin
         Match_Object := Top_Frame.Data_Stack.Last_Element;

         Match_Is_Implicit := Match_Object.all in Runtime_Language_Entity_Type'Class
           and then Runtime_Language_Entity (Match_Object).Is_Implicit;

         Node.As_Call_Expr.F_Called.Traverse (Visit_Expression'Access);
         Called := Pop_Object;

         --  At this point, the matching is of the form:
         --     <implicit self> (Called (F_Args))
         --     <implicit self>.Called (F_Args)
         --     Match_Object (Called (F_Args))
         --     Match_Object.Called (F_Args)
         --  Depending on wether the matched object is implicit or not.

         --  First check if call is a language entity. we are of the form:
         --     field_or_property_name (F_Args)
         -- or
         --     Match_Object (field_or_property_name (F_Args))

         --  First step, if the called object resolved to a language entity,
         --  check that the parameter match this language entity. Here, we can
         --  resolve to various forms:
         --     (implicit self> (an_entity (F_Args))
         --     (implicit self>.an_entity (F_Args)
         --     Match_Object (an_entity (F_Args))
         --     Match_Object.an_entity (F_Args)
         --  We already resolved an_entity in the context of the top entity,
         --  may that be the implicit self or the matched object.
         --  If there's nothing else to check (F_Args), we're done. Otherwise
         --  check that the arg expression matches the entity.

         if Called.all in Runtime_Language_Entity_Type'Class then
            if Node.As_Call_Expr.F_Args.Children_Count = 0 then
               Push_Match_True (Called);
            elsif Node.As_Call_Expr.F_Args.Children_Count = 1 then
               Push_Implicit_Self (Runtime_Language_Entity_Type'Class (Called.all).Value);
               Node.As_Call_Expr.F_Args.Child (1).As_Argument.F_Value.Traverse
                 (Visit_Expression'Access);
               Result := Pop_Object;
               Pop_Object;

               if Result = Match_False then
                  --  The language entity doesn't correspond to the parameters,
                  --  stack False and return.

                  Push_Match_False;
               else
                  Push_Match_True (Called);
               end if;

               return;
            else
               Error ("matching on an entity reference takes only 1 parameter");
            end if;
         end if;

         --  If Called didn't resolved to a language entity, then it's a
         --  predicate to either the implicit self, or the matched object.

         --  If the matched object is a static entity, for example a template
         --  name, we're in the matcher form:
         --     template_name (field_or_property_name (F_Args))
         --  this translates into: "check that self is of type template_name
         --     and has a field field_or_property_name that corresponds to
         --     the arguments F_Args.

         if Match_Object.all in Runtime_Static_Entity_Type'Class then
            Scope := Runtime_Static_Entity (Match_Object).An_Entity;

            if Scope.Children_Indexed.Contains (Node.As_Call_Expr.F_Called.Text) then
               --  At this point, we stacked references to various namespaces.
               --  For example if we have
               --     a.b.template_name (field_or_property_name (F_Args))
               --  a, b and template_name are on the stack.
               --  They will get unstacked when exiting the selectors. We do need to
               --  retreive the value of self earlier in the stack.

               Self := Get_Implicit_Self;

               Selected := Scope.Children_Indexed.Element (Node.As_Call_Expr.F_Called.Text);

               --  TODO: We might be able to go without a pointer in the signature
               --  of match result, avoiding dynamic memory allocation here.
               if Self.Push_Match_Result
                 (new Runtime_Static_Entity_Type'(An_Entity => Selected),
                  Node.As_Call_Expr.F_Args)
               then
                  return;
               end if;
            else
               Error ("'" & Node.As_Call_Expr.F_Called.Text & "' not found in '"
                      & Named_Entity (Runtime_Static_Entity (Match_Object).An_Entity).Name_Node.Text & "'");
            end if;
         end if;

         --  If the matched object is a runtime entity, then we have a matcher
         --  of the form:
         --     entity (template_name (arguments))
         --  or
         --     entity (field_or_property_name (arguments)
         --  We can differenciate the two from the type of Call which can be
         --  already a static entity. If it's not, we'll just get its text.
         --  TODO: that second alternative is a bit weak, it would be better
         --  to be able to retreive the field object when retreiving called.

         if Match_Object.all in Runtime_Language_Entity_Type'Class then
            if Called.all in Runtime_Static_Entity_Type'Class then
                if Runtime_Language_Entity (Match_Object).Value.Push_Match_Result
                 (Called,
                  Node.As_Call_Expr.F_Args)
               then
                  return;
               end if;
            else
               if Runtime_Language_Entity (Match_Object).Value.Push_Match_Result
                 (new Runtime_Field_Reference_Type'
                    (Name => To_Unbounded_Text (Node.As_Call_Expr.F_Called.Text)),
                  Node.As_Call_Expr.F_Args)
               then
                  return;
               end if;
            end if;
         end if;

         -- If none of the above worked, we don't match the call.

         Push_Match_False;
      end Handle_Match;

   begin
      if Top_Frame.Context = Match_Context then
         Handle_Match;
      else
         Node.As_Call_Expr.F_Called.Traverse (Visit_Expression'Access);

         if Top_Frame.Data_Stack.Last_Element.all in Runtime_Call_To_Global_Type'Class then
            Runtime_Call_To_Global_Type'Class (Top_Frame.Data_Stack.Last_Element.all).Analyze_Parameters (Node.As_Call_Expr.F_Args);
         elsif Top_Frame.Data_Stack.Last_Element.all in Runtime_Function_Reference_Type then
            Handle_Function_Call;
         else
            Error ("expected function call or template reference");
         end if;
      end if;
   end Handle_Call;

   procedure Build_Lambda (A_Lambda : Runtime_Lambda; Lambda_Expression : Libtemplatelang.Analysis.Template_Node) is
      function Not_Capture_Identifiers
        (Node : Libtemplatelang.Analysis.Template_Node'Class) return Libtemplatelang.Common.Visit_Status;

      function Capture_Identifiers
        (Node : Libtemplatelang.Analysis.Template_Node'Class) return Libtemplatelang.Common.Visit_Status;

      procedure Capture (Name : Text_Type) is
         Object : Runtime_Object;
      begin
         if Push_Global_Identifier (Name) then
            --  We found a global identifier to record. If not, we expect it
            --  to be resolved later when running the lambda.

            Object := Pop_Object;

            --  If the object is a runtime_language_entity, it may be marked self
            --  or new. We don't want to carry this property over to the lambda
            --  call, so remove it.

            if Object.all in Runtime_Language_Entity_Type then
               A_Lambda.Captured_Symbols.Insert
                 (Name,
                  new Runtime_Language_Entity_Type'(Value => Runtime_Language_Entity_Type (Object.all).Value, others => <>));
            else
               A_Lambda.Captured_Symbols.Insert (Name, Object);
            end if;
         end if;
      end Capture;

      procedure Capture_Group (Index : Integer; Value : Runtime_Object) is
      begin
         Error ("not yet implemented");
      end Capture_Group;

      procedure Capture_Expression (Expression : Libtemplatelang.Analysis.Template_Node) is
      begin
         Expression.Traverse (Capture_Identifiers'Access);
      end Capture_Expression;

      function Capture_Identifiers
        (Node : Libtemplatelang.Analysis.Template_Node'Class) return Libtemplatelang.Common.Visit_Status
      is
         use Libtemplatelang.Analysis;
         use Libtemplatelang.Common;
      begin
         Push_Error_Location (Node);

         case Node.Kind is
            when Template_Selector =>
               Node.As_Selector.F_Left.Traverse (Capture_Identifiers'Access);
               Node.As_Selector.F_Right.Traverse (Not_Capture_Identifiers'Access);

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
        (Node : Libtemplatelang.Analysis.Template_Node'Class) return Libtemplatelang.Common.Visit_Status is
         use Libtemplatelang.Analysis;
         use Libtemplatelang.Common;
      begin
         Push_Error_Location (Node);

         case Node.Kind is
            when Template_Str =>
               Analyze_Replace_String
                 (Node,
                  Capture_Group'Access,
                  Capture_Expression'Access);

               Pop_Object;

               Pop_Error_Location;
               return Over;
            when Template_Call_Expr | Template_New_Expr =>
               --  Resolved the called identifier

               for Arg of Node.As_Call_Expr.F_Args loop
                  Arg.Traverse (Capture_Identifiers'Access);
               end loop;

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
      A_Lambda.Implicit_Self := Get_Implicit_Self;
      A_Lambda.Implicit_New := Get_Implicit_New;
      A_Lambda.Lexical_Scope := Top_Frame.Lexical_Scope;
   end Build_Lambda;

   procedure Run_Lambda (A_Lambda : Runtime_Lambda_Type) is
      Copy_Symbols : Runtime_Object_Maps.Map;
      Result : Runtime_Object;
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
