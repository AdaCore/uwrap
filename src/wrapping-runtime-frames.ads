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

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;

package Wrapping.Runtime.Frames is

   type Matched_Groups_Type;
   type Matched_Groups is access all Matched_Groups_Type;
   package Matched_Groups_Vectors is new Ada.Containers.Vectors
     (Positive, Matched_Groups);
   use Matched_Groups_Vectors;

   type Data_Frame_Type;
   type Data_Frame is access all Data_Frame_Type;
   package Data_Frame_Vectors is new Ada.Containers.Vectors
     (Positive, Data_Frame);
   use Data_Frame_Vectors;

   type Frame_Context_Type;
   type Frame_Context is access all Frame_Context_Type;

   function Top_Frame return Data_Frame with Inline;
   --  Returns the value of the last frame pushed

   function Parent_Frame return Data_Frame with Inline;
   --  Returns the value of the parent of the last frame pushed, null if it's
   --  the root frame.

   function Top_Context return Frame_Context with Inline;
   --  Returns the last context pushed on the top frame.

   type Allocate_Callback_Type is access procedure
     (E : access W_Object_Type'Class);
   --  Profile of callback called when a new node is created within an
   --  expression, as a non-wrapping node.

   type Outer_Expr_Callback_Type is access procedure;
   --  Profile of callback for outer expression processing, expecting the
   --  object to work on at the top of the stack

   type Capture_Mode is (Capture, Rollback);
   --  Through regular expression processing, entities are going through stages
   --  of capturing and rolling back. This type is used to track where we're
   --  on.

   type Capture_Callback_Type is access procedure (Mode : Capture_Mode);
   --  Callback type for capturing values during regular expression processing.

   type Yield_Callback_Type is access procedure;
   --  Callback type for runinning processing following the yield of a value
   --  by a generator.

   type Function_Result_Callback_Type is access procedure;
   --  Callback type for running processing following the result of a function
   --  being picked.

   type Match_Kind is
     (
      Match_None,
      --  We're not doing any match

      Match_Ref_Default,
      --  We are doing a match in the context of a reference, e.g.: match
      --  x, and taking the default behavior in this context. For example,
      --  x.f_name is by default an is match

      Match_Call_Default,
      --  We are doing a match in the context of a call, e.g.: match x, and
      --  taking the default behavior in this context. For example, x.f_name
      --  () is by default an is match. Not that this mode applies both to the
      --  call named matched and its result.

      Match_Is,
      --  Force a match is, typically through a is', e.g. is (x.f_name ())

      Match_Has
      --  Force a match has, typically through a is', e.g. has (x.f_name ())
     );

   type Outer_Expr_Action_Type is
     (Action_None,
      --  Nothing to be done at the end of the expression evaluation

      Action_Match,
      --  A Match with the outer object should be performed.

      Action_Post_Pick_Exec
      --  The post pick section of the current command needs to be executed;.
      --  be either the remaining of a selector or a template clause or a
      --  command sequence. E.g., where a is a function call:
      --     function a () do pick <some expression> end;
      --     a ().left_expr ()
      --  or
      --     pick a () wrap left_expr ();
     );

   type Regexpr_Matcher_Type;
   type Regexpr_Matcher is access all Regexpr_Matcher_Type;

   type Generator_Callback_Type is access procedure (Expr : T_Expr);
   --  Callback used in regular expression processing to generate values
   --  matching a given match expression.

   type Capture_Result_Type;

   type Capture_Result is access all Capture_Result_Type;

   type Data_Frame_Type is record
      Symbols : W_Object_Maps.Map;
      --  These are the runtime symbols that have been capture by this frame
      --  to date.

      Group_Sections : Matched_Groups_Vectors.Vector;
      --  In string regular expressions, groups can be captured by position.
      --  These are stored in this field.

      Data_Stack     : W_Object_Vectors.Vector;
      --  Stack of the data currently being processed.

      Top_Context    : Frame_Context;
      --  Last Context pushed on this frame.

      Lexical_Scope  : T_Entity;
      --  Lexical scope that lead to the creation of this frame, could be the
      --  root object for the global frame, a template or a function.

      Temp_Names : Tmp_Map_Access;
      --  Temporary names generated for this frame, with their string
      --  association.

      Interrupt_Program : Boolean := False;
      --  When set to true, this flag will interrupt the current program, and
      --  either move to the parent frame or simply stop the global program,
      --  moving to the next node to process.

      Current_Template : W_Object;
      --  When the frame is stacked for a template, it's accessible through
      --  this variable

      Template_Parameters_Position : T_Expr_Vectors.Vector;
      --  If this frame is a template, this will store the parameters that were
      --  passed in the order they were provided. Positional parameters are
      --  always provided before named ones.

      Template_Parameters_Names    : T_Expr_Maps.Map;
      --  If this frame is a template, this will store the parameters that
      --  were passed in a name => value convention.
   end record;
   --  This type hold a frame, which can be either the global frame, or a
   --  frame local to a template instance or a function.

   type Frame_Context_Type is record
      Parent_Context : Frame_Context;
      --  The parent context.

      Yield_Callback : Yield_Callback_Type;
      --  This is set by functions that iterate over generators, and called by
      --  generators on each returned value. This should never be called
      --  directly, but instead through the Call_Yield subprogram which will
      --  set the correct frame context.

      Allocate_Callback : Allocate_Callback_Type := null;
      --  Callback used to record objects allocated through the new ()
      --  function. This needs to be set in particular in browsing functions,
      --  in order to be able to capture things such as child (new ()).

      Outer_Expr_Action : Outer_Expr_Action_Type := Action_None;
      --  Some processing may need to be done once reaching an expression
      --  terminals. For example:
      --    X (A or B);
      --  needs to match X against A and against B.
      --     pick a.b wrap C ()
      --  needs to apply the C wrapping to a.b.
      --     pick (a.all () and b) wrap C ()
      --  needs to apply wrapping to the expansion of a and b This flag
      --  identifies what to be done.

      Function_Result_Callback : Function_Result_Callback_Type;
      --  When a function is called, it will generate one or more results.
      --  When generated, these results will be provided to the caller through
      --  this Function_Result_Callback, which will be able to either stack
      --  the object, or evaluate the rest of an expression for which the
      --  function call is a prefix.

      Visit_Decision : Visit_Action_Ptr;
      --  This pointer is a pointer to the variable used by the last node
      --  iteration, and allows to control the decision to take on the next
      --  step. In particular it allows to interrupt a sub-iteration

      Current_Command : T_Command;
      --  The command currently processed

      Match_Mode : Match_Kind := Match_None;
      --  Tracks wether or not we're in a match section, which can be either
      --  the match section of a command, or a matching preducate, e.g. in a
      --  find.

      Name_Captured : Unbounded_Text_Type;
      --  When hitting a capture expression, the name is being stored here so
      --  that the capturing expression can update its value.

      It_Value : W_Object;
      --  Reference to the current iterated value, accessible by "it".

      Left_Value : W_Object;
      --  When set, this identifies the value at the left of the expression.
      --  For example, in A (V => @ & "something"), @ is the left value
      --  refering to V.

      Is_Root_Selection : Boolean := True;
      --  This flag allows to detect if we're on the root selection of an
      --  entity. E.g. in A.B.C (D, E.F), A, D and E are root selections.
      --  This is used to know if we can look at globals when resolving names.

      Outer_Object : W_Object;
      --  The object to around this context. For example, in:
      --     match A (B.C, D);
      --  B.C and D match against A, A matches against self.

      Regexpr_Anchored : Boolean := False;
      --  When processing a regular expressions, this allows to track wether
      --  we're in an anchored section, that is we should not look past the
      --  first level of iteration. For example, for next, this means only
      --  matching the directly next node, for child this means only matching
      --  across the first level children.

      Regexpr : Regexpr_Matcher;
      --  When processing a regular expression, pointer to the current regexpr
      --  matcher.

      Is_First_Matching_Wrapper : Boolean := True;
      --  When iterating over wrappers, we can have several matches on the same
      --  node. During an iteration, this flag matches wether we're calling
      --  visitors on the first wrapper that matches a given condition, or
      --  another one. This is used in particular to avoid duplicates when
      --  computing the children on those wrappers (only an iteration on the
      --  first matching one would be necessary).

      Indent : Integer := 0;
      --  Current indentation when generating text in indent mode, with the
      --  syntax i"".
   end record;
   --  A Frame_Context is a type that is recording stack-based properties that
   --  vary within a given frame, typically through an expression, or various
   --  parts of a command. Each Frame is supposed to start with a fresh frame
   --  context (ie information does not travel through frame contexts).

   type Matched_Groups_Type is record
      Groups : W_Object_Vectors.Vector;
   end record;
   --  Contains a set of matched value by index. It's important to store these
   --  by level. For example in:
   --    match x"(.*) (.*)" do
   --       match x"(.*)" wrap standard.out ("\3");
   --       match x"(a.*b)" wrap standard.out ("\3");
   --    end;
   --  there's only one frame. The first match provides two groups. The second
   --  match provides a third group only valid for that second match, the third
   --  match provides another third group. We need to be able to push and pop
   --  the ones related to the second and third while keeping the first.

   type Capture_Result_Type is record
      Parent : Capture_Result;
      Object : W_Object;
   end record;
   --  This type is used to track the variable in which we're currently
   --  capturing the result of a regular expressions. Such capture is
   --  recursive. E.g. in:
   --     a: many (b: (something) \ c (something))
   --  a is the parent of b and c. When e.g. b is processed, captured object
   --  must be provided to b and a.

   type Regexpr_Matcher_Type is record
      Outer_Next_Expr        : Regexpr_Matcher;
      --  In expressions such as
      --     (b \ c) \ d
      --  When processing "b \ c", the outer next expr is pointing to d so that
      --  it's possible, when reaching the end of the subexpression b \ c, to
      --  retreive and process d.

      Current_Expr           : T_Expr;
      --  This field tracks the expression to analyze in a given frame context.
      --  Stacked frame contexts through the analysis will move this pointer
      --  across the whole expression.

      Generator              : Generator_Callback_Type;
      --  Callback generating values from a given node of a regular expression.
      --  this may generate more than one value, which will be captured by the
      --  yield callback installed by the regular expression processor.

      Overall_Yield_Callback : Yield_Callback_Type;
      --  When entering a regular expression, there may be already a yield
      --  callback installed. However, resolving the regular expression itself
      --  relies on generating values which will be captured through internal
      --  yield callbacks. This allows to keep track of the callback that
      --  was installed before entering the expression, so that in something
      --  such as:
      --     child (a \ b).fold ()
      --  we can store the yield callback that corresponds to fold when
      --  entering the expression, and call it when reaching a successful match
      --  on b. Incenditally, when not null, this means that the regular
      --  expression will generate every possible chain.

      Capturing              : Capture_Result;
      --  The object to which we're currently capturing results

      Quantifiers_Hit        : Integer := 0;
      --  Quantifiers such as many or few may have boundaries (finding at least
      --  and at most certain numbers of matches). This field captures how many
      --  have been found so far and allows to decide wether more need to
      --  be retreived, or wether the number that we have is already enough

      Capture_Installed      : Boolean := False;
      --  Tracks wether the capturing object has already been installed for
      --  the given matcher. This is important in particular as quantifiers
      --  will execute many times, but only needs to install the capture
      --  variable initially.
   end record;
   --  This type is used in regular expressions, to map to a given term of
   --  the expression. For example in:
   --    a \ b \ many (c)
   --  3 of these types will be created by the algorithm one after the other
   --  to keep track of all the data necessary for the processing.

   procedure Call_Yield
     (Callback : Yield_Callback_Type := Top_Context.Yield_Callback)
   with Post => W_Stack_Size = W_Stack_Size'Old;
   --  Calls the current yield callback if set, setting the proper context
   --  around it. Calling this function will replace the element on the top
   --  of the stack by the element stacked by yield. If the Callback is null,
   --  this is a null operation.

   function Get_Local_Symbol (Name : Text_Type) return W_Object;
   --  Return a symbol of a given name local to the top frame, null
   --  if none.

   function Get_Module (A_Frame : Data_Frame_Type) return T_Module;
   --  Return the module enclosing this frame.

   function Top_Object return W_Object with Inline;
   --  Return the object at the top of the current stack.

   procedure Push_Frame (Lexical_Scope : access T_Entity_Type'Class);
   --  Push a new frame as a child of the current one, linked to the scope
   --  in parameter which may be the root scope, a function or a template.

   procedure Push_Frame (Frame : Data_Frame);
   --  Push the frame in parameter on top of the frame list, with the previous
   --  one sets as it parents. This frame may already be in the frame stack
   --  below. This can be used in particular when e.g. function needs to
   --  generate values in the parent frames, then go back to their scope to
   --  continue processing.

   procedure Push_Frame (A_Closure : Closure);
   --  Push a frame re-installing the closure in parameter, for example to
   --  run a deferred command or a deferred expression.

   procedure Pop_Frame;
   --  Pop the last push frame, updating both the Top_Frame and Parent_Frame
   --  references.

   procedure Push_Frame_Context;
   --  Push a new frame context as the exact copy of the top one.

   procedure Push_Frame_Context (Context : Frame_Context_Type);
   --  Push a new frame context as a copy of the one provided in parameter.

   procedure Push_Frame_Context_Parameter;
   --  Push a frame context to prepare a parameter evaluation

   procedure Push_Frame_Context_Parameter_With_Match (Object : W_Object);
   --  Push a frame context to prepare the matching expression of a predicate
   --  against a given outer object.

   procedure Push_Frame_Context_No_Outer;
   --  Push a frame context with no outer object or function

   procedure Push_Frame_Context_No_Match;
   --  Push a frame context with matching deactivated.

   procedure Push_Frame_Context_No_Pick;
   --  Many expression part need to deactivate being picked as function
   --  results. For example, while in:
   --     function f do
   --        pick a and b;
   --     end;
   --  we want to pick a and b (if it's called from e.g. .all(), in :
   --     function f do
   --        pick a & b;
   --     end;
   --  we only want to pick the result of a & b. The following function pushes
   --  a new context with the proper flags.

   procedure Pop_Frame_Context;
   --  Pops the last pushed frame context.

   procedure Push_Match_Groups_Section;
   --  Push a new group section. For example in:
   --    match x"(.*) (.*)" do
   --       match x"(.*)" wrap standard.out ("\3");
   --       match x"(a.*b)" wrap standard.out ("\3");
   --    end;
   --  Each match requires a specific group section as we need to be able to
   --  push and pop the second and third one without altering the first.

   procedure Pop_Match_Groups_Section;
   --  Pops the last pushed group section.

   procedure Push_Object (Object : access W_Object_Type'Class);
   --  Pushes an object to the data stack.

   procedure Push_Implicit_It (Object : access W_Object_Type'Class);
   --  Pushes a reference to the object in parameter as the implicit it
   --  parameter.

   procedure Push_Allocated_Entity (Object : access W_Object_Type'Class);
   --  Pushes a reference to the object in parameter as a newly allocated
   --  entity, which can then be used by certain functions to link to the
   --  structure currently iterated on. For example in:
   --     child (new (something ()))
   --  the allocated callback will recognized the entity as having being
   --  allocated and will set it as a child of the current it object.

   procedure Push_Temporary_Name (Name : Text_Type; Object : W_Object);
   --  Push the temporary of the given name on the stack as a W_String. A
   --  temporary name is defined by:
   --    - a frame
   --    - a node
   --    - a name
   --  in a given frame for a given name and node, the temporary name will
   --  always be the same. If the frame, the node or the name are different,
   --  it will be unique. This uniqueness is guaranteed by the node counter,
   --  incremented every time a new name is created for that node and used
   --  in the temporary name.
   --  TODO: This is not enough to guarantee uniqueness! Two objects may have
   --  the same name and same counter for the same frame. To be fixed.

   procedure Pop_Object;
   --  Pops the last object pushed to the stack.

   procedure Pop_Underneath_Top;
   --  Deletes the object right below the top object

   function Pop_Object return W_Object;
   --  Pops the last object pushed on the stack and returns it.

   function Get_Implicit_It return W_Object;
   --  Returns last pushed implicit it for the frame in parameter

private

   Top_Object_Ref   : W_Object;
   Top_Frame_Ref    : Data_Frame;
   Parent_Frame_Ref : Data_Frame;

   function Top_Object return W_Object is (Top_Object_Ref);

   function Top_Frame return Data_Frame is (Top_Frame_Ref);

   function Parent_Frame return Data_Frame is (Parent_Frame_Ref);

   function Top_Context return Frame_Context is
     (Top_Frame_Ref.Top_Context);

end Wrapping.Runtime.Frames;
