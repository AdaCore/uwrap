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
-- General Public License  distributed with UWrap; see file COPYING3.  If --
-- not, go to http://www.gnu.org/licenses for a complete copy of the        --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO;           use Ada.Wide_Wide_Text_IO;
with Ada.Containers;                  use Ada.Containers;
with Ada.Characters.Conversions;      use Ada.Characters.Conversions;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Fixed;     use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings;                     use Ada.Strings;
with Ada.Tags;                        use Ada.Tags;
with GNAT.Regpat;                     use GNAT.Regpat;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with Langkit_Support.Diagnostics;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libtemplatelang.Common; use Libtemplatelang.Common;

with Wrapping.Regex;               use Wrapping.Regex;
with Wrapping.Semantic.Analysis;   use Wrapping.Semantic.Analysis;
with Wrapping.Semantic.Structure;  use Wrapping.Semantic.Structure;
with Wrapping.Utils;               use Wrapping.Utils;
with Wrapping.Runtime.Functions;   use Wrapping.Runtime.Functions;
with Wrapping.Runtime.Strings;     use Wrapping.Runtime.Strings;
with Wrapping.Runtime.Frames;      use Wrapping.Runtime.Frames;
with Wrapping.Runtime.Expressions; use Wrapping.Runtime.Expressions;
with Wrapping.Runtime.Matching;    use Wrapping.Runtime.Matching;
with Wrapping.Runtime.Closures;    use Wrapping.Runtime.Closures;

package body Wrapping.Runtime.Analysis is

   Visitor_Counter : Integer := 0;
   --  This is the counter of visitor. Every time a visitor is started
   --  (including the main one), it is to be incremented. This provdes a unique
   --  id to each visit execution, which later allows to check that a language
   --  entity isn't visited twice by the same visitor invokation.

   Current_Visitor_Id : Integer := 0;
   --  The Id for the current visitor, updated when entering a vistor
   --  invokation. Note that the main iteration is always id 0. TODO:
   --  maybe this should be frame information?

   Deferred_Commands : Deferred_Command_Vectors.Vector;

   procedure Handle_Command_Nodefer (Command : T_Command) with
     Post => W_Stack_Size = W_Stack_Size'Old;
   --  Same as Handle_Command_Front but skips the defer part of the command,
   --  computing its actions right away. This assumes that the command
   --  context has been previously installed and will be cleared afterwards
   --  (see Install_Command_Context / Uninstall_Command_Context).

   procedure Handle_Command_Back (Command : T_Command) with
     Post => W_Stack_Size = W_Stack_Size'Old;

   function Analyze_Visitor
     (E : access W_Object_Type'Class; Result : out W_Object)
      return Wrapping.Semantic.Structure.Visit_Action;

   -------------------------
   -- Push_Error_Location --
   -------------------------

   procedure Push_Error_Location (An_Entity : access T_Entity_Type'Class) is
   begin
      Push_Error_Location
        (An_Entity.Unit.Get_Filename, Start_Sloc (An_Entity.Sloc));
   end Push_Error_Location;

   ------------------------
   -- Pop_Error_Location --
   ------------------------

   procedure Pop_Error_Location is
   begin
      Wrapping.Pop_Error_Location;
   end Pop_Error_Location;

   ---------------------------
   -- Apply_Template_Action --
   ---------------------------

   procedure Apply_Template_Action
     (It : W_Node; Template_Clause : T_Template_Section)
   is
      A_Template_Instance : W_Template_Instance;
      Self_Weave          : Boolean := False;
      Result              : W_Object;

      --  TODO: is this still necessary?
      Dummy_Action : Visit_Action;
   begin
      Push_Error_Location (Template_Clause.Node);
      Push_Implicit_It (It);

      if Template_Clause.Call.Is_Null then
         --  We've set a null template - the objective is to prevent this
         --  entity to be wrapped by this template.

         if Template_Clause.Call.Args.Length /= 1 then
            Error ("expected one argument to null");
         end if;

         Push_Frame_Context;

         --  There's nothing to check on the expression below. Deactivate the
         --  expression callback (otherwise, it may perform wrong calls, either
         --  to an unwanted check, or to the outer pick function.
         Top_Frame.Top_Context.Outer_Expr_Callback := null;

         Evaluate_Expression (Template_Clause.Call.Args.Element (1).Expr);
         Result := Pop_Object.Dereference;
         Pop_Frame_Context;

         if Result.all not in W_Static_Entity_Type'Class
           or else W_Static_Entity (Result).An_Entity.all not in
             T_Template_Type'Class
         then
            Error ("expected template reference");
         end if;

         It.Forbidden_Template_Names.Include
           (T_Template (W_Static_Entity (Result).An_Entity).Full_Name);

         --  TODO: remove the template if it's already been created in the
         --  context of a weave clause
      else
         if Template_Clause.Call.Reference = null then
            --  No name to the call, that means that we're expecting to
            --  self-weave the current template.
            if Template_Clause.Kind /= Weave_Kind then
               Error
                 ("self wrap not allowed, either weave or"
                  & " provide a template or visitor name");
            elsif It.all in W_Template_Instance_Type'Class then
               A_Template_Instance := W_Template_Instance (It);
            else
               Error ("only template instances can be self weaved");
            end if;

            Self_Weave := True;
         elsif Template_Clause.Call.Reference.all in T_Template_Type'Class then
            A_Template_Instance :=
              It.Get_Template_Instance
                (T_Template (Template_Clause.Call.Reference));

            if Template_Clause.Kind = Walk_Kind then
               A_Template_Instance :=
                 It.Create_Template_Instance
                   (T_Template (Template_Clause.Call.Reference), False);
            elsif
              (Template_Clause.Kind = Weave_Kind
               or else A_Template_Instance = null
               or else A_Template_Instance.Is_Wrapping = False)
              and then not It.Forbidden_Template_Names.Contains
                (Template_Clause.Call.Reference.Full_Name)
            then
               if A_Template_Instance = null then
                  A_Template_Instance :=
                    It.Create_Template_Instance
                      (T_Template (Template_Clause.Call.Reference), True);
               end if;

               if Template_Clause.Kind = Wrap_Kind then
                  A_Template_Instance.Is_Wrapping := True;
               end if;
            else
               A_Template_Instance := null;
            end if;
         else
            Error ("unexpected template call reference type");
         end if;

         if A_Template_Instance /= null then
            if Template_Clause.Call.Captured_Name /= "" then
               Include_Symbol
                 (To_Text (Template_Clause.Call.Captured_Name),
                  W_Object (A_Template_Instance));
            end if;

            Dummy_Action :=
              Handle_Template_Call
                (W_Object (A_Template_Instance), Template_Clause.Call.Args);
         end if;
      end if;

      Pop_Object;
      Pop_Error_Location;
   end Apply_Template_Action;

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

      Pop_Object; -- Pop It.
      Pop_Frame;
   end Handle_Command;

   -----------------------
   -- Allocate_Detached --
   -----------------------

   procedure Allocate_Detached (E : access W_Object_Type'Class) is
   begin
      --  when allocating an object outside of a browsing function, nothign
      --  special to do
      null;
   end Allocate_Detached;

   -----------------------------
   -- Install_Command_Context --
   -----------------------------

   procedure Install_Command_Context (Command : T_Command) is
   begin
      Top_Frame.Top_Context.Allocate_Callback :=
        Allocate_Detached'Unrestricted_Access;
      Top_Frame.Top_Context.Outer_Expr_Callback :=
        Outer_Expression_Match'Access;
      Top_Frame.Top_Context.Current_Command   := Command;
      Top_Frame.Top_Context.Is_Root_Selection := True;
      Top_Frame.Top_Context.Outer_Object      := Top_Object;

      Push_Match_Groups_Section;
   end Install_Command_Context;

   -------------------------------
   -- Uninstall_Command_Context --
   -------------------------------

   procedure Uninstall_Command_Context is
   begin
      Pop_Match_Groups_Section;
   end Uninstall_Command_Context;

   --------------------------
   -- Handle_Command_Front --
   --------------------------

   procedure Handle_Command_In_Current_Frame (Command : T_Command) is
   begin
      if Top_Frame.Interrupt_Program then
         return;
      end if;

      if Command.Defer then
         declare
            New_Command : Deferred_Command := new Deferred_Command_Type;
         begin
            New_Command.Command   := Command;
            New_Command.A_Closure :=
              Capture_Closure (Command.Deferred_Closure);

            Deferred_Commands.Append (New_Command);
         end;
      else
         Install_Command_Context (Command);
         Handle_Command_Nodefer (Command);
         Uninstall_Command_Context;
      end if;
   end Handle_Command_In_Current_Frame;

   ----------------------------------
   -- Handle_Command_Front_Nodefer --
   ----------------------------------

   procedure Handle_Command_Nodefer (Command : T_Command) is
   begin
      if Evaluate_Match_Expression (Command.Match_Expression) then
         if Command.Pick_Expression /= null then
            --  When evaluating a pick expression, the wrapping program will
            --  be evaluated by the outer epxression callback. This caters
            --  in particular for cases where more than one object is being
            --  retreived.

            Top_Frame.Top_Context.Outer_Expr_Callback :=
              Outer_Expression_Pick'Access;

            Evaluate_Expression (Command.Pick_Expression);
            Pop_Object;

            Top_Frame.Top_Context.Outer_Expr_Callback :=
              Outer_Expression_Match'Access;
         else
            Handle_Command_Back (Command);
         end if;
      else
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

                  if Evaluate_Match_Expression (Else_Section.Match_Expression)
                  then
                     Handle_Command_Sequence (Else_Section);
                     exit;
                  end if;

                  Else_Section := Else_Section.Next_Element;
               end loop;
            end;
         end if;
      end if;
   end Handle_Command_Nodefer;

   -------------------------
   -- Handle_Command_Back --
   -------------------------

   procedure Handle_Command_Back (Command : T_Command) is
      Top : W_Object := Top_Object.Dereference;
      It  : W_Node;
   begin
      if Top_Frame.Interrupt_Program then
         return;
      end if;

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
         Handle_Command_Sequence (Command.Command_Sequence.First_Element);
      elsif Command.Template_Section /= null then
         if Command.Template_Section.A_Visit_Action /= Unknown then
            --  TODO: consider differences between weave and wrap here
            --  TODO: This doesn't consider different visits, each should have
            --  its own decision

            if Top_Frame.Top_Context.Visit_Decision.all = Unknown then
               Top_Frame.Top_Context.Visit_Decision.all :=
                 Command.Template_Section.A_Visit_Action;
            end if;
         elsif Command.Template_Section.Call.Reference /= null
           or else Command.Template_Section.Call.Args.Length /= 0
         then
            --  There is an explicit template call. Pass this on either the
            --  current template or the whole tree

            Apply_Template_Action (It, Command.Template_Section);
         end if;
      end if;

      Pop_Object;
   end Handle_Command_Back;

   ----------------------------
   -- Handle_Defered_Command --
   ----------------------------

   function Handle_Defered_Command (Command : Deferred_Command) return Boolean
   is
      Result : Boolean := False;
   begin
      Push_Frame (Command.A_Closure);

      Install_Command_Context (Command.Command);

      --  Only commands with a command sequence and no else part can be defered

      if Evaluate_Match_Expression (Command.Command.Defer_Expression) then
         Handle_Command_Nodefer (Command.Command);
         Result := True;
      end if;

      Uninstall_Command_Context;

      Pop_Frame;

      return Result;
   end Handle_Defered_Command;

   -----------------------------
   -- Handle_Command_Sequence --
   -----------------------------

   procedure Handle_Command_Sequence (Sequence : T_Command_Sequence_Element) is
      Seq           : T_Command_Sequence_Element := Sequence;
      Calling_Frame : Data_Frame                 := Parent_Frame;
      Called_Frame  : Data_Frame                 := Top_Frame;
      New_Ref       : W_Reference;
   begin
      while Seq /= null loop
         --  First, create variables for this scope. This need to be done
         --  before any expression evaluation, as initialization and parameter
         --  expressions may reference other variables declared before. Also
         --  see Handle_Template_Call for details on the evaluation of template
         --  parameters.

         for A_Var of Seq.Vars loop
            declare
               Name : Text_Type := A_Var.Name_Node.Text;
            begin
               New_Ref := new W_Reference_Type;

               case A_Var.Kind is
                  when Text_Kind =>
                     --  Symbols contained in templates are references to
                     --  values. Create the reference and the referenced
                     --  empty value here.

                     New_Ref.Value := new W_Text_Vector_Type;

                  when Set_Kind =>
                     New_Ref.Value := new W_Set_Type;

                  when Map_Kind =>
                     New_Ref.Value := new W_Map_Type;

                  when Vector_Kind =>
                     New_Ref.Value := new W_Vector_Type;

                  when Object_Kind =>
                     New_Ref.Value := Match_False;

                  when others =>
                     Error ("variable kind not supported for templates");

               end case;

               Include_Symbol (Name, W_Object (New_Ref));

               if Top_Frame.Current_Template /= null then
                  W_Template_Instance (Top_Frame.Current_Template)
                    .Indexed_Variables
                    .Insert
                    (Name, New_Ref);
                  W_Template_Instance (Top_Frame.Current_Template)
                    .Ordered_Variables
                    .Append
                    (New_Ref);
               end if;
            end;
         end loop;

         --  Second, call initializations

         for A_Var of Seq.Vars loop
            declare
               Name : Text_Type := A_Var.Name_Node.Text;
            begin
               New_Ref := W_Reference (Top_Frame.Symbols.Element (Name));

               if A_Var.Init_Expr /= null then
                  Push_Frame_Context_No_Match;
                  Top_Frame.Top_Context.Left_Value        := New_Ref.Value;
                  Top_Frame.Top_Context.Is_Root_Selection := True;

                  Evaluate_Expression (A_Var.Init_Expr);

                  Pop_Frame_Context;

                  New_Ref.Value := Pop_Object;
               end if;
            end;
         end loop;

         --  Third, call parameter expressions. This needs to be done in the
         --  context of the calling frame, which is temporarily restored for
         --  this purpose. Calling frame may be null here, e.g. in the case
         --  of a defered command. In this case, there's no way to modify the
         --  value of the variable.

         if Calling_Frame /= null then
            Push_Frame (Calling_Frame);

            for A_Var of Seq.Vars loop
               declare
                  Name : Text_Type := A_Var.Name_Node.Text;
               begin
                  New_Ref := W_Reference (Called_Frame.Symbols.Element (Name));

                  if Calling_Frame.Template_Parameters_Position.Length > 0
                    or else Calling_Frame.Template_Parameters_Names.Contains
                      (Name)
                  then
                     Push_Frame_Context_No_Match;
                     Top_Frame.Top_Context.Left_Value        := New_Ref.Value;
                     Top_Frame.Top_Context.Is_Root_Selection := True;

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

   ---------------------
   -- Analyze_Visitor --
   ---------------------

   function Analyze_Visitor
     (E : access W_Object_Type'Class; Result : out W_Object)
      return Visit_Action
   is
      A_Visit_Action : Visit_Action         := Into;
      N              : W_Node;
      Visit_Result   : aliased Visit_Action := Unknown;
   begin
      Push_Frame (Wrapping.Semantic.Analysis.Root);
      Top_Frame.Top_Context.Visit_Decision := Visit_Result'Unchecked_Access;

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

         Pop_Frame;
         return Over;
      else
         --  Otherwise, stack this visitor Id and visit. We need not to remove
         --  this id at the end, as to make sure that potential iterations on
         --  this visit don't cover this node again

         N.Visited_Stack.Append (Current_Visitor_Id);

         Apply_Wrapping_Program (N, Wrapping.Semantic.Analysis.Root);

         Pop_Frame;

         if Visit_Result = Unknown then
            return Into;
         else
            return Visit_Result;
         end if;
      end if;
   end Analyze_Visitor;

   -------------------
   -- Analyse_Input --
   -------------------

   procedure Analyse_Input (Root_Entity : W_Node) is
      Dummy_Action    : Visit_Action;
      Traverse_Result : W_Object;
   begin
      --  Set the visitor id - we're on the main iteration, id is 0.

      Current_Visitor_Id := 0;

      Push_Frame (Wrapping.Semantic.Analysis.Root);
      Dummy_Action :=
        Root_Entity.Traverse
          (Child_Depth, True, Traverse_Result, Analyze_Visitor'Access);
      Pop_Frame;
   end Analyse_Input;

   -----------------------
   -- Analyze_Templates --
   -----------------------

   procedure Analyze_Templates is
      A_Template_Instance : W_Template_Instance;
      Dummy_Action        : Visit_Action;
      Traverse_Result     : W_Object;

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
                  if not Handle_Defered_Command (D) then
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

               Dummy_Action := Analyze_Visitor (T, Traverse_Result);
            end loop;
         end;
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
   end Analyze_Templates;

   ----------------------------
   -- Outer_Expression_Match --
   ----------------------------

   procedure Outer_Expression_Match is
   begin
      if Top_Frame.Top_Context.Match_Mode not in Match_None | Match_Has then
         --  If we're matching, and we're not forcing the "has" mode, then
         --  check that the initial object we had on the stack matches the
         --  new one.

         if not Top_Frame.Top_Context.Outer_Object.Match_With_Top_Object then
            Pop_Object;
            Push_Match_False;
         end if;
      end if;
   end Outer_Expression_Match;

   ---------------------------
   -- Outer_Expression_Pick --
   ---------------------------

   procedure Outer_Expression_Pick is
   begin
      if Top_Frame.Top_Context.Pick_Callback /= null
        and then Top_Frame.Top_Context.Current_Command.Command_Sequence = null
      then
         --  We are on a final pick expression (not followed by a command
         --  sequence). The Pick_Callback contains what to do with the
         --  picked object.
         Top_Frame.Top_Context.Pick_Callback (Top_Object);
      else
         Handle_Command_Back (Top_Frame.Top_Context.Current_Command);
      end if;
   end Outer_Expression_Pick;

end Wrapping.Runtime.Analysis;
