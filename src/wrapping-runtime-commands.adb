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

with Ada.Containers.Vectors;
with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;
with Ada.Containers;                    use Ada.Containers;
with Ada.Characters.Conversions;        use Ada.Characters.Conversions;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;

with Wrapping.Semantic.Analysis;   use Wrapping.Semantic.Analysis;
with Wrapping.Utils;               use Wrapping.Utils;
with Wrapping.Runtime.Strings;     use Wrapping.Runtime.Strings;
with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;
with Wrapping.Runtime.Matching;    use Wrapping.Runtime.Matching;
with Wrapping.Runtime.Closures;    use Wrapping.Runtime.Closures;
with Wrapping.Runtime.Commands;    use Wrapping.Runtime.Commands;
with Wrapping.Runtime.Objects;     use Wrapping.Runtime.Objects;

package body Wrapping.Runtime.Commands is

   type Deferred_Command_Type;
   type Deferred_Command is access all Deferred_Command_Type;
   package Deferred_Command_Vectors is new Ada.Containers.Vectors
     (Positive, Deferred_Command);
   use Deferred_Command_Vectors;

   type Deferred_Command_Type is record
      Command   : T_Command;
      --  The command that was deferred.

      A_Closure : Closure;
      --  The closure capturing the data to re-load when running the command.
   end record;
   --  This type record the necessary data to store and re-launch a defered
   --  command.

   Current_Visitor_Id : Integer := 0;
   --  The Id for the current visitor, updated when entering a vistor
   --  invokation. Note that the main iteration is always id 0. TODO:
   --  maybe this should be frame information?

   Deferred_Commands : Deferred_Command_Vectors.Vector;
   --  The list of commands deferred to the next iteration

   Templates_To_Traverse : W_Template_Instance_Vectors.Vector;
   --  The list of templates created by this iteration, on which the program
   --  will be run durnign the next iteration.

   procedure Handle_Command_Post_Defer (Command : T_Command) with
     Post => W_Stack_Size = W_Stack_Size'Old;
   --  Same as Handles a command after consideration of its defer clause.
   --  This assumes that the command context has been previously installed and
   --  will be cleared afterwards (see Install_Command_Context /
   --  Uninstall_Command_Context).

   function Run_Main_Program (Node : W_Node) return Visit_Action;
   --  Runs the main program on the Node in parameter, returns any visit
   --  decision that may have been taken.

   procedure Apply_Template_Section (Template_Section : T_Template_Section);
   --  Executes the template section (weave/wrap/walk) of a given command

   procedure Install_Command_Context (Command : T_Command);
   --  Installs the context prior to running a command

   procedure Uninstall_Command_Context;
   --  Installs the context prior to running a command

   function Run_Defered_Command (Command : Deferred_Command) return Boolean;
   --  Evaluates the condition of a deferred command. If sucessful, runs that
   --  command and returns true. Returns false otherwise.

   ---------------------------
   -- Apply_Template_Action --
   ---------------------------

   procedure Apply_Template_Section (Template_Section : T_Template_Section)
   is
      It                  : constant W_Node := W_Node (Get_Implicit_It);
      A_Template_Instance : W_Template_Instance;
      Result              : W_Object;
   begin
      Push_Error_Location (Template_Section.Node);

      if Template_Section.Call.Is_Null then
         --  We've set a null template - the objective is to prevent this
         --  entity to be wrapped by this template.

         if Template_Section.Call.Args.Length /= 1 then
            Error ("expected one argument to null");
         end if;

         Push_Frame_Context;

         --  There's nothing to check on the expression below. Deactivate the
         --  expression callback (otherwise, it may perform wrong calls, either
         --  to an unwanted check, or to the outer pick function.

         Top_Context.Outer_Expr_Action := Action_None;

         --  Retreives the template that needs to be forbidden

         Evaluate_Expression (Template_Section.Call.Args.Element (1).Expr);
         Result := Pop_Object.Dereference;

         Pop_Frame_Context;

         --  Checks that we indeed retreived a template

         if Result.all not in W_Static_Entity_Type'Class
           or else W_Static_Entity (Result).An_Entity.all not in
             T_Template_Type'Class
         then
            Error ("expected template reference");
         end if;

         --  If the template was not previously instantiated, then forbids its
         --  creation.

         declare
            Name : constant Text_Type :=
              T_Template (W_Static_Entity (Result).An_Entity).Full_Name;
         begin
            if not It.Wrappers_By_Full_Id.Contains (Name) then
               It.Forbidden_Wrapper_Types.Include (Name);
            end if;
         end;
      else
         --  We're on a wrap / weave / walk template section

         if Template_Section.Call.Reference = null then
            --  No name to the call, that means that we're expecting to
            --  self-weave the current template. Check that the object is
            --  a template instance (we can't self weave input nodes) and that
            --  this is indeed a weave clause

            if Template_Section.Kind /= Weave_Kind then
               Error
                 ("self wrap not allowed, either weave or"
                  & " provide a template name");
            elsif It.all not in W_Template_Instance_Type'Class then
               Error ("only template instances can be self weaved");
            else
               A_Template_Instance := W_Template_Instance (It);
            end if;
         else
            --  We're operating with an actual template name. Create or
            --  retreive the instance

            if Template_Section.Kind = Walk_Kind then
               --  Walk always create a new instance, but this instance
               --  will not be stored.

               A_Template_Instance :=
                 Create_Template_Instance
                   (Template_Section.Call.Reference, It, False);
            else
               --  Weave and wrap need to consider possible pre-existing
               --  instance

               A_Template_Instance :=
                 It.Get_Wrapper (Template_Section.Call.Reference);

               --  Create the template if null and not forbidden

               if A_Template_Instance = null
                 and then not It.Forbidden_Wrapper_Types.Contains
                   (Template_Section.Call.Reference.Full_Name)
               then
                  A_Template_Instance :=
                    Create_Template_Instance
                      (Template_Section.Call.Reference, It, True);
               end if;

               --  If we have an instance and we're on a wrap clause, check
               --  that this instance wasn't already processed in the context
               --  of a wrap clause, and set the wrap flag to true in that case
               --  to prevent further processing in cases of wrap clauses.

               if A_Template_Instance /= null and then
                 Template_Section.Kind = Wrap_Kind
               then
                  if A_Template_Instance.Is_Wrapping then
                     A_Template_Instance := null;
                  else
                     A_Template_Instance.Is_Wrapping := True;
                  end if;
               end if;
            end if;
         end if;

         --  If we found an instance, store its reference to the captured name
         --  of the clause if any, then process its call.

         if A_Template_Instance /= null then
            if not (Template_Section.Call.Captured_Name = "") then
               Include_Symbol
                 (To_Text (Template_Section.Call.Captured_Name),
                  W_Object (A_Template_Instance));
            end if;

            Handle_Template_Call
              (W_Object (A_Template_Instance), Template_Section.Call.Args);
         end if;
      end if;

      Pop_Error_Location;
   end Apply_Template_Section;

   --------------------
   -- Handle_Command --
   --------------------

   procedure Handle_Command (Command : T_Command; It : W_Node) is
   begin
      --  The command is the enclosing scope for all of its clauses. It will in
      --  particular receive the matching groups and the temporary values that
      --  can be used consistently in the various clauses

      Push_Frame (Command);
      Push_Implicit_It (It);

      Handle_Command_In_Current_Frame (Command);

      Pop_Object;
      Pop_Frame;
   end Handle_Command;

   -----------------------------
   -- Install_Command_Context --
   -----------------------------

   procedure Install_Command_Context (Command : T_Command) is
   begin
      Top_Context.Do_Allocate       := True;
      Top_Context.Outer_Expr_Action := Action_Match;
      Top_Context.Current_Command   := Command;
      Top_Context.Is_Root_Selection := True;
      Top_Context.Outer_Object      := Top_Object;

      Push_Match_Groups_Section;
   end Install_Command_Context;

   -------------------------------
   -- Uninstall_Command_Context --
   -------------------------------

   procedure Uninstall_Command_Context is
   begin
      Pop_Match_Groups_Section;
   end Uninstall_Command_Context;

   -------------------------------------
   -- Handle_Command_In_Current_Frame --
   -------------------------------------

   procedure Handle_Command_In_Current_Frame (Command : T_Command) is
   begin
      --  If this is a deferred command, capture the closure and schedule it.
      --  Otherwise, install the context and run the command.

      if Command.Defer then
         declare
            New_Command : constant Deferred_Command :=
              new Deferred_Command_Type;
         begin
            New_Command.Command   := Command;
            New_Command.A_Closure :=
              Capture_Closure (Command.Deferred_Closure);

            Deferred_Commands.Append (New_Command);
         end;
      else
         Install_Command_Context (Command);
         Handle_Command_Post_Defer (Command);
         Uninstall_Command_Context;
      end if;
   end Handle_Command_In_Current_Frame;

   ----------------------------
   -- Handle_Command_Nodefer --
   ----------------------------

   procedure Handle_Command_Post_Defer (Command : T_Command) is
   begin
      --  Evaluate the match expression (which will be null if there's no
      --  match section) then either runs the rest of the command or look
      --  for else alteratives.

      if Evaluate_Match (Command.Match_Expression) then
         --  This command matched, evaluate pick if needed or go directly
         --  to the post pick section if there's none.

         if Command.Pick_Expression /= null then
            --  When evaluating a pick expression, the rest of the command
            --  will be evaluated by the outer action. This handles
            --  in particular for cases where more than one object is being
            --  retreived and that action is done in the yeild callback.

            Push_Frame_Context;
            Top_Context.Outer_Expr_Action := Action_Post_Pick_Exec;

            Evaluate_Expression (Command.Pick_Expression);

            --  Throw away the result of the pick expression, it was taken
            --  care of as part of the post pick action.
            Pop_Object;

            Pop_Frame_Context;
         else
            --  If there's no pick, we're already on the right "it" object and
            --  cand execute directly the post pick part of the command.

            Handle_Command_Post_Pick (Command);
         end if;
      else
         --  This command did not match. If there is a command sequence, e.g.:
         --     match <some expression do
         --        <some actions>
         --     else
         --        <some other actions>
         --     end;
         --  Look for the elsmatch and else sections one by one. Execute the
         --  commands of the first one that matches

         if Command.Command_Sequence /= null then
            declare
               Else_Section : T_Command_Sequence_Element :=
                 Command.Command_Sequence.First_Element;
            begin
               while Else_Section /= null loop
                  while Else_Section /= null and then not Else_Section.Is_Else
                  loop
                     Else_Section := Else_Section.Next_Element;
                  end loop;

                  exit when Else_Section = null;

                  --  We found the begining of an else section. Evaluate its
                  --  match expression (it will be null if just an else and
                  --  not an elsmatch). If it works, then handle the command
                  --  sequence starting at this point, up until the next else
                  --  or the end of the sequence, then exit.

                  if Evaluate_Match (Else_Section.Match_Expression) then
                     Handle_Command_Sequence (Else_Section);

                     exit;
                  end if;

                  Else_Section := Else_Section.Next_Element;
               end loop;
            end;
         end if;
      end if;
   end Handle_Command_Post_Defer;

   ------------------------------
   -- Handle_Command_Post_Pick --
   ------------------------------

   procedure Handle_Command_Post_Pick (Command : T_Command) is
      Top : constant W_Object := Top_Object.Dereference;
      It  : W_Node;
   begin
      --  Start by processing the top of the stack to retreive the expected
      --  implicit "it" object, and possibly interrupt the processing if
      --  false or not one of the acceptable forms.

      if Top = Match_False then
         return;
      elsif Top.all in W_Node_Type'Class then
         It := W_Node (Top);
      elsif Top.all in W_Static_Entity_Type then
         It :=
           W_Node (Get_Object_For_Entity (W_Static_Entity (Top).An_Entity));
      else
         Error ("can't pick selected object");
      end if;

      Push_Implicit_It (It);

      if Command.Command_Sequence /= null then
         --  If there's a command sequence of the form do .. end, handle
         --  from the first element until an else, elsmatch or end is hit.

         Handle_Command_Sequence (Command.Command_Sequence.First_Element);
      elsif Command.Template_Section /= null then
         --  If we're on a template section (wrap / weave / walk), process its
         --  various forms.

         if Command.Template_Section.A_Visit_Action /= Unknown then
            --  We're on a wrap <some decision> section. Update the visit
            --  decision.

            --  We should always be in the context of an iteration at this
            --  point, and there should always be the possibilty to alter
            --  the traversal.

            pragma Assert (Top_Context.Visit_Decision /= null);

            if Top_Context.Visit_Decision.all = Unknown then
               Top_Context.Visit_Decision.all :=
                 Command.Template_Section.A_Visit_Action;
            end if;
         else
            --  We're either creating or updating a template. Process the
            --  requested action.

            Apply_Template_Section (Command.Template_Section);
         end if;
      end if;

      Pop_Object;
   end Handle_Command_Post_Pick;

   ----------------------------
   -- Handle_Defered_Command --
   ----------------------------

   function Run_Defered_Command (Command : Deferred_Command) return Boolean
   is
      Result : Boolean := False;
   begin
      Push_Frame (Command.A_Closure);

      Install_Command_Context (Command.Command);

      --  Only commands with a command sequence and no else part can be defered

      if Evaluate_Match (Command.Command.Defer_Expression) then
         Handle_Command_Post_Defer (Command.Command);
         Result := True;
      end if;

      Uninstall_Command_Context;

      Pop_Frame;

      return Result;
   end Run_Defered_Command;

   -----------------------------
   -- Handle_Command_Sequence --
   -----------------------------

   procedure Handle_Command_Sequence (Sequence : T_Command_Sequence_Element) is
      Seq           : T_Command_Sequence_Element := Sequence;
      Calling_Frame : constant Data_Frame        := Parent_Frame;
      Called_Frame  : constant Data_Frame        := Top_Frame;
      New_Ref       : W_Reference;
   begin
      --  Command sequences are implemented as a list of section separated by
      --  "then", "else" or "elsematch". Process iteratively all element from
      --  the current one up until hitting one that is not linked as a "then".

      while Seq /= null loop
         --  First, create variables for this scope. This need to be done
         --  before any expression evaluation, as initialization and parameter
         --  expressions may reference other variables declared before. Also
         --  see Handle_Template_Call for details on the evaluation of template
         --  parameters.

         for A_Var of Seq.Vars loop
            declare
               Name : constant Text_Type := A_Var.Name_Node.Text;
            begin
               --  Variables stored in templates are modeled through
               --- references, so that code can refer to one value and get
               --  updated when the field is updated.

               --  TODO: Do we really need that with defer expressions
               --  all over?

               New_Ref := new W_Reference_Type;

               case A_Var.Kind is
                  when Text_Kind =>
                     New_Ref.Value := new W_String_Type;

                  when Set_Kind =>
                     New_Ref.Value := new W_Set_Type;

                  when Map_Kind =>
                     New_Ref.Value := new W_Map_Type;

                  when Vector_Kind =>
                     New_Ref.Value := new W_Vector_Type;

                  when Object_Kind =>
                     --  TODO: it's strange to use Match_False as a default
                     --  here (but it seems to have functional consequences in
                     --  some of the tests). Consider a W_Object_Type instead.

                     New_Ref.Value := Match_False;

                  when others =>
                     Error ("variable kind not supported for templates");

               end case;

               Include_Symbol (Name, W_Object (New_Ref));

               if Top_Frame.Current_Template /= null then
                  W_Template_Instance (Top_Frame.Current_Template)
                    .Indexed_Variables.Insert (Name, New_Ref);
                  W_Template_Instance (Top_Frame.Current_Template)
                    .Ordered_Variables.Append (New_Ref);
               end if;
            end;
         end loop;

         --  Second, call initializations in the context of the current
         --  template instance.

         for A_Var of Seq.Vars loop
            declare
               Name : constant Text_Type := A_Var.Name_Node.Text;
            begin
               New_Ref := W_Reference (Get_Local_Symbol (Name));

               if A_Var.Init_Expr /= null then
                  Push_Frame_Context_No_Match;
                  Top_Context.Left_Value        := New_Ref.Value;
                  Top_Context.Is_Root_Selection := True;

                  --  TODO: we should have some type checking here, accepting
                  --  either a W_Defer or a value that matches the expected
                  --  type
                  New_Ref.Value := Evaluate_Expression (A_Var.Init_Expr);

                  Pop_Frame_Context;
               end if;
            end;
         end loop;

         --  Third, call parameter expressions. This needs to be done in the
         --  context of the calling frame, which is temporarily restored for
         --  this purpose. Calling frame may be null here, e.g. in the case
         --  of a defered command. This means in particular that parameters
         --  passed to a template cannot value the variable of a deferred
         --  command, e.g. in:
         --
         --     template X do
         --        defer do
         --           var Y : Integer;
         --        end;
         --     end;
         --
         --     wrap X (0); # error

         if Calling_Frame /= null then
            --  Pushes the calling frame back to the top.

            Push_Frame (Calling_Frame);

            --  Looks at all the variables just created an initializes, and
            --  see if there's a parameter expression to apply.

            for A_Var of Seq.Vars loop
               declare
                  Name : constant Text_Type := A_Var.Name_Node.Text;
               begin
                  New_Ref := W_Reference (Called_Frame.Symbols.Element (Name));

                  --  If there's at least a by-position parameter left to use,
                  --  or if there's a by-name parameter of the correct name,
                  --  use it to evaluate the current variable

                  if Calling_Frame.Template_Parameters_Position.Length > 0
                    or else Calling_Frame.Template_Parameters_Names.Contains
                      (Name)
                  then
                     Push_Frame_Context_No_Match;
                     Top_Context.Left_Value        := New_Ref.Value;
                     Top_Context.Is_Root_Selection := True;

                     if Calling_Frame.Template_Parameters_Position.Length > 0
                     then
                        Evaluate_Expression
                          (Calling_Frame.Template_Parameters_Position
                             .First_Element);
                        Calling_Frame.Template_Parameters_Position
                          .Delete_First;
                     else
                        Evaluate_Expression
                          (Calling_Frame.Template_Parameters_Names.Element
                             (Name));
                        Calling_Frame.Template_Parameters_Names.Delete (Name);
                     end if;

                     New_Ref.Value := Pop_Object;

                     Pop_Frame_Context;
                  end if;
               end;
            end loop;

            Pop_Frame;
         end if;

         --  Then execute command in reverse

         for C of reverse Seq.Commands loop
            exit when Top_Frame.Interrupt_Program;

            Handle_Command_In_Current_Frame (T_Command (C));
         end loop;

         exit when Top_Frame.Interrupt_Program;

         Seq := Seq.Next_Element;

         --  Run all command sequences up until hitting an else section. If
         --  such section is hit, it's controlled by a top level match section,
         --  which already validated the current sequence as matching. The else
         --  wasn't picked. Stop runnint here.

         exit when Seq = null or else Seq.Is_Else;
      end loop;
   end Handle_Command_Sequence;

   ----------------------------
   -- Apply_Wrapping_Program --
   ----------------------------

   procedure Apply_Wrapping_Program
     (It : W_Node; Lexical_Scope : access T_Entity_Type'Class)
   is
   begin
      Push_Frame (Lexical_Scope);

      for Wrapping_Entity of reverse Lexical_Scope.Children_Ordered loop
         if Wrapping_Entity.all in T_Command_Type then
            Handle_Command (T_Command (Wrapping_Entity), It);
         elsif Wrapping_Entity.all in T_Module_Type'Class
           or else Wrapping_Entity.all in T_Namespace_Type'Class
         then
            Apply_Wrapping_Program (It, Wrapping_Entity);
         end if;
      end loop;

      Pop_Frame;
   end Apply_Wrapping_Program;

   --------------------------------
   -- Register_Template_Instance --
   --------------------------------

   procedure Register_Template_Instance (Instance : W_Template_Instance) is
   begin
      Templates_To_Traverse.Append (Instance);
   end Register_Template_Instance;

   ---------------------
   -- Analyze_Visitor --
   ---------------------

   function Run_Main_Program (Node : W_Node) return Visit_Action
   is
      Visit_Result   : aliased Visit_Action := Unknown;
   begin
      Push_Frame (Wrapping.Semantic.Analysis.Root);
      Top_Context.Visit_Decision := Visit_Result'Unchecked_Access;

      --  Check if this entity has already been analyzed through this visitor
      --  invocation.

      --  First, pop any id in the entity that may be greater than the current
      --  one. If they are greater and we're on a lower one, it means that they
      --  are over.

      if Node.all not in W_Node_Type'Class then
         Error ("expected node type");
      end if;

      while Node.Visited_Stack.Length > 0
        and then Node.Visited_Stack.Last_Element > Current_Visitor_Id
      loop
         Node.Visited_Stack.Delete_Last;
      end loop;

      if Node.Visited_Stack.Length > 0
        and then Node.Visited_Stack.Last_Element = Current_Visitor_Id
      then
         --  If the last id in the stack is the current one, they we've already
         --  visited this entity. We already also made the decisions on sub
         --  entities. Stop the iteration.

         Pop_Frame;
         return Over;
      else
         --  Otherwise, stack this visitor Id and visit. We need not to remove
         --  this id at the end, as to make sure that potential iterations on
         --  this visit don't cover this node again

         Node.Visited_Stack.Append (Current_Visitor_Id);

         Apply_Wrapping_Program (Node, Wrapping.Semantic.Analysis.Root);

         Pop_Frame;

         if Visit_Result = Unknown then
            return Into;
         else
            return Visit_Result;
         end if;
      end if;
   end Run_Main_Program;

   -------------------
   -- Analyse_Input --
   -------------------

   procedure Analyse_Input (Root_Entity : W_Node) is

      function Visitor
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action;

      -------------
      -- Visitor --
      -------------

      function Visitor
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action
      is
      begin
         Result := null;
         return Run_Main_Program (W_Node (E));
      end Visitor;

      Dummy_Action    : Visit_Action;
      Traverse_Result : W_Object;

   begin
      --  Set the visitor id - we're on the main iteration, id is 0.

      Current_Visitor_Id := 0;

      Push_Frame (Wrapping.Semantic.Analysis.Root);
      Dummy_Action :=
        Root_Entity.Traverse
          (Child_Depth, True, Traverse_Result, Visitor'Access);
      Pop_Frame;
   end Analyse_Input;

   -----------------------
   -- Analyze_Templates --
   -----------------------

   procedure Analyzed_Deferred is
      A_Template_Instance : W_Template_Instance;
      A_Visit_Action      : Visit_Action;

      Files         : W_Template_Instance_Vectors.Vector;
      Output        : W_Template_Instance_Vectors.Vector;
      File_Template : T_Template;
      Out_Template  : T_Template;
   begin
      File_Template :=
        T_Template
          (Resolve_Module_By_Name ("standard").Children_Indexed.Element
             ("file"));

      Out_Template :=
        T_Template
          (Resolve_Module_By_Name ("standard").Children_Indexed.Element
             ("out"));

      while Templates_To_Traverse.Length > 0
        or else Deferred_Commands.Length > 0
      loop
         declare
            Next_Iteration : W_Template_Instance_Vectors.Vector;
         begin
            Next_Iteration.Move (Templates_To_Traverse);
            Templates_To_Traverse.Clear;

            --  Reset the visitor id - we're on the main iteration, id is 0.

            for Created_Template of Next_Iteration loop
               A_Template_Instance := W_Template_Instance (Created_Template);

               if Instance_Of
                   (T_Template (A_Template_Instance.Defining_Entity),
                    File_Template)
               then
                  Files.Append (A_Template_Instance);
               elsif Instance_Of
                   (T_Template (A_Template_Instance.Defining_Entity),
                    Out_Template)
               then
                  Output.Append (A_Template_Instance);
               end if;
            end loop;

            --  Analyzed all defered commands that are valid

            declare
               Next_Deferred : Deferred_Command_Vectors.Vector;
            begin
               for D of Deferred_Commands loop
                  if not Run_Defered_Command (D) then
                     Next_Deferred.Append (D);
                  end if;
               end loop;

               --  TODO: This is incorrect, additional deferred commands may
               --  have been created above and will be ignored here.
               Deferred_Commands.Move (Next_Deferred);
            end;

            Current_Visitor_Id := 0;

            for T of Next_Iteration loop
               --  The newly created wrappers need to be analyzed in order of
               --  creation. So we're not using the traverse function anymore,
               --  but instead just go through the list.

               A_Visit_Action := Run_Main_Program (W_Node (T));
               exit when A_Visit_Action = Stop;
            end loop;
         end;

         exit when A_Visit_Action = Stop;
      end loop;

      Buffer.Full_Cursor_Update := True;

      for T of Output loop
         declare
            Content_Object : W_Object;
         begin
            Push_Frame (Wrapping.Semantic.Analysis.Root);

            if not T.Push_Value ("content") then
               Error ("'content' component not found in file template");
            end if;

            Content_Object := Pop_Object;

            Push_Buffer_Cursor;

            declare
               Slice : Buffer_Slice;
            begin
               Slice := Content_Object.Write_String;

               Put
                 (Buffer.Str
                    (Slice.First.Offset .. Slice.Last.Offset));
            end;

            Pop_Buffer_Cursor;

            Pop_Frame;
         end;
      end loop;

      for T of Files loop
         declare
            Path_Object    : W_Object;
            Content_Object : W_Object;
            Output_File    : File_Type;
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

            Push_Buffer_Cursor;

            declare
               Slice : Buffer_Slice;
            begin
               Slice := Path_Object.Write_String;

               Create
                 (Output_File,
                  Out_File,
                  To_String (Buffer.Str
                    (Slice.First.Offset .. Slice.Last.Offset)));
            end;

            Pop_Buffer_Cursor;

            Push_Buffer_Cursor;

            declare
               Slice : Buffer_Slice;
            begin
               Slice := Content_Object.Write_String;
               Put (Output_File,
                    Buffer.Str
                      (Slice.First.Offset .. Slice.Last.Offset));
            end;

            Pop_Buffer_Cursor;

            Close (Output_File);

            Pop_Frame;
         end;
      end loop;
   end Analyzed_Deferred;

end Wrapping.Runtime.Commands;
