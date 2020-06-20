with Libtemplatelang;
with Libtemplatelang.Analysis;

with Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;

package Wrapping.Runtime.Analysis is

   Top_Frame : Data_Frame;

   Templates_To_Traverse : W_Template_Instance_Vectors.Vector;

   procedure Analyse (Root_Entity : W_Node);

   -- TODO: Make a version of that function that pops and return the result.
   -- it should be used in many places
   procedure Evaluate_Expression
     (Node : Libtemplatelang.Analysis.Template_Node'Class);

   function Analyze_Visitor
     (E : access W_Object_Type'Class;
      Result : out W_Object) return Wrapping.Semantic.Structure.Visit_Action;

   function Match (Pattern, Text : Text_Type) return Boolean;
   --  Match a pattern with a text, adding group and captured variables on the
   --  top frame

   procedure Push_Frame_Context;

   procedure Pop_Frame_Context;

   procedure Push_Object (An_Object : access W_Object_Type'Class);

   procedure Push_Implicit_Self (An_Entity : access W_Object_Type'Class);

   procedure Push_Implicit_New (An_Entity : access W_Object_Type'Class);

   procedure Push_Allocated_Entity (An_Entity : access W_Object_Type'Class);

   --  Push the temporary of the given name on the stack. If one needs to be
   --  created, counter will be used to append to the name and be incremented.
   procedure Push_Temporary_Name (Name : Text_Type; Counter : in out Integer);

   --  TODO: generalize the usage of this instead of manual deleting from the
   --  stack
   procedure Pop_Object (Number : Positive := 1);

   function Pop_Object return W_Object;

   --  Same as top object, but if the top object is a rerefence, recursively
   --  traverse all the reference links to reach the first non-reference object.
   function Pop_Object_Dereference return W_Object;

   function Top_Object return W_Object;

   --  Same as top object, but if the top object is a rerefence, recursively
   --  traverse all the reference links to reach the first non-reference object.
   function Top_Object_Dereference return W_Object;

   --  Return True if the top object on the frame is an implicitely stacked
   --  object
   function Top_Is_Implicit return Boolean;

   procedure Run_Lambda (A_Lambda : W_Lambda_Type);

   --  This is the counter of visitor. Every time a visitor is started
   --  (including the main one), it is to be incremented. This provdes a
   --  unique id to each visit execution, which later allows to check that
   --  a language entity isn't visited twice by the same visitor invokation.
   Visitor_Counter : Integer := 0;

   --  The Id for the current visitor, updated when entering a vistor invokation.
   --  Note that the main iteration is always id 0.
   --  TODO: maybe this should be frame information?
   Current_Visitor_Id : Integer := 0;

end Wrapping.Runtime.Analysis;
