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

with Ada.Containers.Vectors;

with Wrapping.Utils;              use Wrapping.Utils;
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

   function Parent_Frame return Data_Frame with Inline;

   function Top_Context return Frame_Context with Inline;

   type Allocate_Callback_Type is access procedure
     (E : access W_Object_Type'Class);

   type Outer_Expr_Callback_Type is access procedure;

   type Capture_Mode is (Capture, Rollback);

   type Capture_Callback_Type is access procedure (Mode : Capture_Mode);

   type Yield_Callback_Type is access procedure;

   type Function_Result_Callback_Type is access procedure (Object : W_Object);

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

   type Visit_Action_Ptr is access all Visit_Action;

   type Regexpr_Matcher_Type;

   type Regexpr_Matcher is access all Regexpr_Matcher_Type;

   type Generator_Type is access procedure (Expr : T_Expr);

   type Capture_Result_Type;

   type Capture_Result is access all Capture_Result_Type;

   type Data_Frame_Type is record
      Symbols : W_Object_Maps.Map;

      Group_Sections : Matched_Groups_Vectors.Vector;
      Data_Stack     : W_Object_Vectors.Vector;
      Top_Context    : Frame_Context;
      Lexical_Scope  : T_Entity;

      Temp_Names : Text_Maps_Access;

      Interrupt_Program : Boolean := False;

      Current_Template : W_Object;
      --  When the frame is stacked for a template, it's accessible through
      --  this variable

      Template_Parameters_Position : T_Expr_Vectors.Vector;
      Template_Parameters_Names    : T_Expr_Maps.Map;
   end record;

   type Frame_Context_Type is record
      Parent_Context : Frame_Context;

      Yield_Callback : Yield_Callback_Type;
      --  This is set by functions that iterate over generators, and called by
      --  generators on each returned value. This should never be called
      --  directly, but instead through the Call_Yield subprogram which will
      --  set the correct frame context.

      Allocate_Callback : Allocate_Callback_Type := null;
      --  Callback used to record objects allocated through the new ()
      --  function. This needs to be set in particular in browsing functions,
      --  in order to be able to capture things such as child (new ()).

      Outer_Expr_Callback : Outer_Expr_Callback_Type;
      --  Some processing may need to be done once reaching an expression
      --  terminals. For example:
      --    X (A or B);
      --  needs to match X against A and against B.
      --     pick a.b wrap C ()
      --  needs to apply the C wrapping to a.b.
      --     pick (a.all () and b) wrap C ()
      --  needs to apply wrapping to the expansion of a and b The type below
      --  allows to idenrify this callback

      Function_Result_Callback : Function_Result_Callback_Type;
      --  When a function is called, it will generate one or more results.
      --  When generated, these results will be provided to the caller through
      --  this Function_Result_Callback, which will be able to either stack
      --  the object, or evaluate the rest of an expression for which the
      --  function call is a prefix.

      Visit_Decision : Visit_Action_Ptr;

      Current_Command : T_Command;
      --  The command currently processed

      Match_Mode : Match_Kind := Match_None;

      Name_Captured : Unbounded_Text_Type;
      --  When hitting a capture expression, the name is being stored here so
      --  that the capturing expression can update its value.

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

      Regexpr : Regexpr_Matcher;

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

   type Capture_Result_Type is record
      Parent : Capture_Result;
      Object : W_Object;
   end record;

   type Regexpr_Matcher_Type is record
      Outer_Next_Expr        : Regexpr_Matcher;
      Current_Expr           : T_Expr;
      Generator              : Generator_Type;
      Overall_Yield_Callback : Yield_Callback_Type;
      Capturing              : Capture_Result;
      Generator_Decision     : Visit_Action := Unknown;
      Quantifiers_Hit        : Integer := 0;
      Capture_Installed      : Boolean := False;
   end record;

   procedure Call_Yield
     (Callback : Yield_Callback_Type := Top_Context.Yield_Callback)
   with Post => W_Stack_Size = W_Stack_Size'Old;
   --  Calls the current yield callback if set, setting the proper context
   --  around it. Calling this function will replace the element on the top
   --  of the stack by the element stacked by yield. If the Callback is null,
   --  this is a null operation.

   function Get_Visible_Symbol
     (A_Frame : Data_Frame_Type; Name : Text_Type) return W_Object;

   function Get_Module
     (A_Frame : Data_Frame_Type) return Semantic.Structure.T_Module;

   function Top_Object return W_Object with Inline;
   --  Return the object at the top of the current stack.

   procedure Push_Frame (Lexical_Scope : access T_Entity_Type'Class);

   procedure Push_Frame (Frame : Data_Frame);

   procedure Push_Frame (A_Closure : Closure);

   procedure Pop_Frame;

   procedure Push_Frame_Context;

   procedure Push_Frame_Context (Context : Frame_Context_Type);

   procedure Push_Frame_Context_Parameter;

   procedure Push_Frame_Context_Parameter_With_Match (Object : W_Object);

   procedure Push_Frame_Context_No_Outer;

   procedure Push_Frame_Context_No_Match;

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

   procedure Push_Match_Groups_Section;

   procedure Pop_Match_Groups_Section;

   procedure Push_Object (Object : access W_Object_Type'Class);

   procedure Push_Implicit_It (Object : access W_Object_Type'Class);

   procedure Push_Allocated_Entity (Object : access W_Object_Type'Class);

   procedure Push_Temporary_Name (Name : Text_Type; Counter : in out Integer);
   --  Push the temporary of the given name on the stack. If one needs to be
   --  created, counter will be used to append to the name and be incremented.

   procedure Pop_Object (Number : Positive := 1);

   procedure Delete_Object_At_Position (Position : Integer) with
     Pre => Position /= 0;
   --  Deletes a specific object. Negative deletes from end, positives delete
   --  from start

   function Pop_Object return W_Object;

   function Top_Is_Implicit return Boolean;
   --  Return True if the top object on the frame is an implicitely stacked
   --  object

   function Get_Implicit_It (From : Data_Frame := Top_Frame) return W_Object;

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
