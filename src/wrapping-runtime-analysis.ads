with Ada.Containers; use Ada.Containers;

with Libtemplatelang;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;
with Wrapping.Utils; use Wrapping.Utils;

package Wrapping.Runtime.Analysis is

   Templates_To_Traverse : W_Template_Instance_Vectors.Vector;

   Deferred_Commands : Deferred_Command_Vectors.Vector;

   procedure Analyse_Input (Root_Entity : W_Node);

   procedure Analyze_Templates;

   procedure Evaluate_Expression (Expr : T_Expr)
     with Post => Top_Frame.Data_Stack.Length =
       Top_Frame.Data_Stack.Length'Old + 1;

   function Evaluate_Expression (Expr : T_Expr) return W_Object;

   function Analyze_Visitor
     (E : access W_Object_Type'Class;
      Result : out W_Object) return Wrapping.Semantic.Structure.Visit_Action;

   function Match (Pattern, Text : Text_Type) return Boolean;
   --  Match a pattern with a text, adding group and captured variables on the
   --  top frame

   procedure Push_Frame (Lexical_Scope : access T_Entity_Type'Class);

   procedure Push_Frame (Frame : Data_Frame);

   procedure Push_Frame (A_Closure : Closure);

   procedure Pop_Frame;

   procedure Push_Frame_Context;

   procedure Push_Frame_Context (Context : Frame_Context_Type);

   procedure Push_Frame_Context_Parameter;

   --  Many expression part need to deactivate being picked as function results.
   --  For example, while in:
   --     function f do
   --        pick a and b;
   --     end;
   --  we want to pick a and b (if it's called from e.g. .all(), in :
   --     function f do
   --        pick a & b;
   --     end;
   --  we only want to pick the result of a & b.
   --  The following function pushes a new context with the proper flags.
   procedure Push_Frame_Context_No_Pick;

   procedure Pop_Frame_Context;

   procedure Push_Match_Groups_Section;

   procedure Pop_Match_Groups_Section;

   procedure Push_Object (Object : access W_Object_Type'Class)
     with Pre =>
       (if Object.all in W_Reference_Type'Class then W_Reference (Object).Value /= null);

   procedure Push_Implicit_It (Object : access W_Object_Type'Class);

   procedure Push_Allocated_Entity (Object : access W_Object_Type'Class);

   --  Push the temporary of the given name on the stack. If one needs to be
   --  created, counter will be used to append to the name and be incremented.
   procedure Push_Temporary_Name (Name : Text_Type; Counter : in out Integer);

   procedure Pop_Object (Number : Positive := 1);

   --  Deletes a specific object. Negative deletes from end, positives delete
   --  from start
   procedure Delete_Object_At_Position (Position : Integer)
     with Pre => Position /= 0;

   function Pop_Object return W_Object;

   Top_Object : W_Object;

   --  Return True if the top object on the frame is an implicitely stacked
   --  object
   function Top_Is_Implicit return Boolean;

   function Get_Implicit_It (From : Data_Frame := Top_Frame) return W_Object;

   function Capture_Closure (Names : Text_Sets.Set) return Closure;

   procedure Capture_Deferred_Environment (Deferred_Expr : W_Deferred_Expr; Expr : T_Expr);

   procedure Run_Deferred_Expr (Deferred_Expr : W_Deferred_Expr_Type);

   --  This is the counter of visitor. Every time a visitor is started
   --  (including the main one), it is to be incremented. This provdes a
   --  unique id to each visit execution, which later allows to check that
   --  a language entity isn't visited twice by the same visitor invokation.
   Visitor_Counter : Integer := 0;

   --  The Id for the current visitor, updated when entering a vistor invokation.
   --  Note that the main iteration is always id 0.
   --  TODO: maybe this should be frame information?
   Current_Visitor_Id : Integer := 0;

   procedure Outer_Expression_Match;

   procedure Outer_Expression_Pick;

   procedure Handle_Command_Sequence (Sequence : T_Command_Sequence_Element)
     with Post => Top_Frame.Data_Stack.Length =
       Top_Frame.Data_Stack.Length'Old;

end Wrapping.Runtime.Analysis;
