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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers;                  use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Utils;              use Wrapping.Utils;

package Wrapping.Runtime.Structure is
   --  The purpose is to provide the data structures created live during the
   --  wrapping process (e.g. call stacks, template instances, etc.)

   type W_Object_Type;
   type W_Object is access all W_Object_Type'Class;

   function Lt_Wrapper (Left, Right : W_Object) return Boolean;
   function Eq_Wrapper (Left, Right : W_Object) return Boolean;

   package W_Object_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Text_Type, W_Object);
   use W_Object_Maps;
   package W_Object_Vectors is new Ada.Containers.Vectors (Positive, W_Object);
   use W_Object_Vectors;
   package W_Object_Sets is new Ada.Containers.Ordered_Sets
     (W_Object, Lt_Wrapper, Eq_Wrapper);
   use W_Object_Sets;
   package W_Object_Any_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (W_Object, W_Object, Lt_Wrapper, Eq_Wrapper);
   use W_Object_Any_Maps;

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

   Data_Frame_Stack : Data_Frame_Vectors.Vector;

   Top_Frame    : Data_Frame;
   Parent_Frame : Data_Frame;

   type Frame_Context_Type;
   type Frame_Context is access all Frame_Context_Type;

   type Allocate_Callback_Type is access procedure
     (E : access W_Object_Type'Class);

   type Outer_Expr_Callback_Type is access procedure;

   type Capture_Mode is (Capture, Rollback);

   type Capture_Callback_Type is access procedure (Mode : Capture_Mode);

   type Match_Kind is
     (
      --  We're not doing any match
      Match_None,

      --  We are doing a match in the context of a reference, e.g.: match
      --  x, and taking the default behavior in this context. For example,
      --  x.f_name is by default an is match
      Match_Ref_Default,

      --  We are doing a match in the context of a call, e.g.: match x, and
      --  taking the default behavior in this context. For example, x.f_name
      --  () is by default an is match. Not that this mode applies both to the
      --  call named matched and its result. TODO: document that subtelty, or
      --  revisit. e.g. x.a(), a() and its result is matched by the same mode.
      --  This is due to w_Tmpl () where we are matching w_Tmpl(), or name ()
      --  where we're matching both name and its result (which is essentially
      --  here the same).
      Match_Call_Default,

      --  Force a match is, typically through a is', e.g. is (x.f_name ())
      Match_Is,

      --  Force a match has, typically through a is', e.g. has (x.f_name ())
      Match_Has);

   type Yield_Callback_Type is access procedure;

   type Visit_Action_Ptr is access all Visit_Action;

   type Pick_Callback_Type is access procedure (Object : W_Object);

   type Regexpr_Matcher_Type;

   type Regexpr_Matcher is access all Regexpr_Matcher_Type;

   type Generator_Type is access procedure (Expr : T_Expr);

   type Capture_Result_Type;

   type Capture_Result is access all Capture_Result_Type;

   type Text_Buffer_Cursor;

   type Buffer_Slice;

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

   --  A Frame_Context is a type that is recording stack-based properties that
   --  vary within a given frame, typically through an expression, or various
   --  parts of a command. Each Frame is supposed to start with a fresh frame
   --  context (ie information does not travel through frame contexts).
   type Frame_Context_Type is record
      Parent_Context : Frame_Context;

      --  Some processing may need to be done once reaching an expression
      --  terminals. For example:
      --    X (A or B);
      --  needs to match X against A and against B.
      --     pick a.b wrap C ()
      --  needs to apply the C wrapping to a.b.
      --     pick (a.all () and b) wrap C ()
      --  needs to apply wrapping to the expansion of a and b The type below
      --  allows to idenrify this callback
      Outer_Expr_Callback : Outer_Expr_Callback_Type;

      --  The command currently processed
      Current_Command : T_Command;

      Match_Mode : Match_Kind := Match_None;

      --  When hitting a capture expression, the name is being stored here so
      --  that the capturing expression can update its value.
      Name_Captured : Unbounded_Text_Type;

      --  This is set by functions that iterate over generators, and called by
      --  generators on each returned value. This should never be called
      --  directly, but instead through the Call_Yield subprogram which will
      --  set the correct frame context.
      Yield_Callback : Yield_Callback_Type;

      --  Callback used to record objects allocated through the new ()
      --  function. This needs to be set in particular in browsing functions,
      --  in order to be able to capture things such as child (new ()).
      Allocate_Callback : Allocate_Callback_Type := null;

      --  When set, this identifies the value at the left of the expression.
      --  For example, in A (V => @ & "something"), @ is the left value
      --  refering to V.
      Left_Value : W_Object;

      --  This flag allows to detect if we're on the root selection of an
      --  entity. E.g. in A.B.C (D, E.F), A, D and E are root selections.
      --  This is used to know if we can look at globals when resolving names.
      Is_Root_Selection : Boolean := True;

      --  The object to around this context. For example, in:
      --     match A (B.C, D);
      --  B.C and D match against A, A matches against self.
      Outer_Object : W_Object;

      Visit_Decision : Visit_Action_Ptr;

      Regexpr_Anchored : Boolean := False;

      --  When set, this designates the callback to call upon pick. If the
      --  result is Stop, then stop the analysis, otherwise continues.
      Pick_Callback : Pick_Callback_Type;

      Regexpr : Regexpr_Matcher;

      --  When iterating over wrappers, we can have several matches on the same
      --  node. During an iteration, this flag matches wether we're calling
      --  visitors on the first wrapper that matches a given condition, or
      --  another one. This is used in particular to avoid duplicates when
      --  computing the children on those wrappers (only an iteration on the
      --  first matching one would be necessary).
      Is_First_Matching_Wrapper : Boolean := True;

      Indent : Integer := 0;
      --  Current indentation when generating text in indent mode, with the
      --  syntax i"".
   end record;

   type Matched_Groups_Type is record
      Groups : W_Object_Vectors.Vector;
   end record;

   type Text_Maps_Access is access all Text_Maps.Map;

   type Data_Frame_Type is record
      Symbols : W_Object_Maps.Map;

      Group_Sections : Matched_Groups_Vectors.Vector;
      Data_Stack     : W_Object_Vectors.Vector;
      Top_Context    : Frame_Context;
      Lexical_Scope  : T_Entity;

      Temp_Names : Text_Maps_Access;

      Interrupt_Program : Boolean := False;

      --  When the frame is stacked for a template, it's accessible through
      --  this variable
      Current_Template : W_Object;

      Template_Parameters_Position : T_Expr_Vectors.Vector;
      Template_Parameters_Names    : T_Expr_Maps.Map;
   end record;

   procedure Call_Yield
     (Callback : Yield_Callback_Type := Top_Frame.Top_Context.Yield_Callback)
   with Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;
   --  Calls the current yield callback if set, setting the proper context
   --  around it. Calling this function will replace the element on the top
   --  of the stack by the element stacked by yield. If the Callback is null,
   --  this is a null operation.

   function Get_Visible_Symbol
     (A_Frame : Data_Frame_Type; Name : Text_Type) return W_Object;

   function Get_Module
     (A_Frame : Data_Frame_Type) return Semantic.Structure.T_Module;

   type Closure_Type;
   type Closure is access all Closure_Type;

   type Deferred_Command_Type;
   type Deferred_Command is access all Deferred_Command_Type;
   package Deferred_Command_Vectors is new Ada.Containers.Vectors
     (Positive, Deferred_Command);
   use Deferred_Command_Vectors;

   --  This is the root type of all values that are manipulated by expressions
   type W_Object_Type is tagged record
      null;
   end record;

   procedure Include_Symbol (Name : Text_Type; Object : not null W_Object);

   function Push_Value
     (An_Entity : access W_Object_Type; Name : Text_Type) return Boolean is
     (False) with
      Post'Class => Top_Frame.Data_Stack.Length'Old =
      (if Push_Value'Result then Top_Frame.Data_Stack.Length - 1
       else Top_Frame.Data_Stack.Length);

   procedure Push_Call_Result
     (An_Entity : access W_Object_Type; Params : T_Arg_Vectors.Vector)
     with Post'Class => Top_Frame.Data_Stack.Length =
       Top_Frame.Data_Stack.Length'Old + 1;
   --  Calling an entity means either doing an actual call if this entity
   --  refers to a function, or performing a comparison between the object
   --  and the provided parameters. In the second case, by convention, the
   --  result of the call (last object on the stack) is either Match_False,
   --  or Match_True (An_Entity). By default, this returns an error (the
   --  object is not made for being called).

   function Is_Generator (An_Entity : access W_Object_Type) return Boolean
   is (False);
   --  If this returns true, then the Push_Call_Result call before is
   --  responsible to call the yield callback, if any. Otherwise, this is done
   --  by the caller.

   procedure Generate_Values (Object : access W_Object_Type; Expr : T_Expr);
   --  If this object is a container or a generator, will generate values
   --  matching the expression given in parameter one by one, calling the
   --  yield action. If no Yield action is set in the frame, only generate
   --  the first matching one, false if none.

   function Match_With_Top_Object
     (An_Entity : access W_Object_Type) return Boolean;
   --  Match this object with the top of the stack. Return False if no decision
   --  could be made, true otherwise. If the top object doesn't match, replace
   --  it with a match false.

   type Browse_Mode is
     (Parent, Child_Depth, Child_Breadth, Next, Prev, Sibling, Wrapper);

   --  TODO: This is probably actually needed only at the node level.
   function Traverse
     (An_Entity  : access W_Object_Type; A_Mode : Browse_Mode;
      Include_It : Boolean; Final_Result : out W_Object;
      Visitor    : access function
        (E : access W_Object_Type'Class; Result : out W_Object)
         return Visit_Action)
      return Visit_Action;

   --  TODO this into push browse_Result? And see with the other push browse
   --  result what should be changed
   procedure Evaluate_Bowse_Functions
     (An_Entity        : access W_Object_Type; A_Mode : Browse_Mode;
      Match_Expression : T_Expr) is null;

   pragma Warnings (Off, "postcondition does not mention");
   function Browse_Entity
     (Browsed :     access W_Object_Type'Class; Match_Expression : T_Expr;
      Result  : out W_Object) return Visit_Action with
      Post => Top_Frame.Data_Stack.Length = Top_Frame.Data_Stack.Length'Old;
   pragma Warnings (On, "postcondition does not mention");

   procedure Evaluate_Generator_Regexp
     (Root      : access W_Object_Type'Class;
      Expr      : T_Expr;
      Generator : Generator_Type);

   procedure Push_Match_True (An_Entity : access W_Object_Type'Class);

   procedure Push_Match_False;

   function Write_String (Object : W_Object_Type) return Buffer_Slice;
   --  This function resolves a runtime object into the String value, and
   --  writes this string in the global string buffer. The result of this
   --  function varies over time - as the underlying object gets completed by
   --  various wrapping and weaving opertions. This should be called as late as
   --  possible in the process (unless explicitely requested through e.g. a
   --  string conversion). Keeping a reference to the runtime object is
   --  prefered. Note that this is directly linked to the actual semantics of
   --  the language, so should remain consistent with it.

   function To_Debug_String (Object : W_Object_Type) return Text_Type
     is ("<empty>");

   function Dereference (Object : access W_Object_Type) return W_Object is
     (W_Object (Object));
   --  If Object represents a reference, returns the referenced object
   --  (recursively) otherwise self.

   function Lt
     (Left : access W_Object_Type; Right : access W_Object_Type'Class)
      return Boolean;

   function Eq
     (Left : access W_Object_Type; Right : access W_Object_Type'Class)
      return Boolean;

   Null_Object : constant W_Object := new W_Object_Type;

   Match_False : constant W_Object := Null_Object;

   function Get_Object_For_Entity
     (An_Entity : access T_Entity_Type'Class) return W_Object;

   type Parameter is record
      Name        : Unbounded_Text_Type;
      Is_Optional : Boolean;
   end record;

   type Parameter_Profile is array (Positive range <>) of Parameter;

   type Actuals_Type is array (Positive range <>) of T_Expr;

   function Make_Parameter
     (Name : Text_Type; Is_Optional : Boolean) return Parameter;

   function Process_Parameters
     (Profile : Parameter_Profile; Arg : T_Arg_Vectors.Vector)
      return Actuals_Type;

   function Evaluate_Match_Result
     (Object : W_Object; Matching_Expression : T_Expr) return Boolean;

   procedure Push_Match_Result
     (Object : W_Object; Matching_Expression : T_Expr);

   procedure Push_Match_It_Result
     (It : W_Object; Matching_Expression : T_Expr);

   procedure Handle_Call_Parameters
     (Args               : T_Arg_Vectors.Vector;
      Evaluate_Parameter : access procedure
        (Name : Text_Type; Position : Integer; Value : T_Expr));

   type Closure_Type is record
      Captured_Symbols : W_Object_Maps.Map;
      Implicit_It      : W_Object;
      Lexical_Scope    : T_Entity;
      Temp_Names       : Text_Maps_Access;
      Left_Value       : W_Object;
   end record;

   type Deferred_Command_Type is record
      Command   : T_Command;
      A_Closure : Closure;
   end record;

   type Text_Buffer_Cursor is record
      Offset      : Natural;
      --  Offset refering to position in Buffer.Str

      Line        : Natural;
      --  Current line where this cursor is located. This is only updated
      --  if Buffer.Full_Cursor_Update is true.

      Line_Offset : Natural;
      --  Current line character offset where this cursor is located. This is
      --  only updated if Buffer.Full_Cursor_Update is true.

      Column      : Natural;
      --  Current column where this cursor is located. This is only updated if
      --  Buffer.Full_Cursor_Update is true.
      --  TODO: tabs are not currently supported.

      Max_Column  : Natural;
      --  Max column that has been encountered so far in this buffer position.
   end record;
   --  Used to point to a position in the text buffer.

   type Buffer_Slice is record
      First, Last : Text_Buffer_Cursor;
   end record;
   --  Represents a slice of data in the main text buffer, First and Last are
   --  inclusive. Empty slice have a last cursor before the first cursor.

end Wrapping.Runtime.Structure;
