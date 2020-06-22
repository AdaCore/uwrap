with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Utils; use Wrapping.Utils;

package Wrapping.Runtime.Structure is
   -- The purpose is to provide the data structures created live during the
   -- wrapping process (e.g. call stacks, template instances, etc.)

   type W_Object_Type;
   type W_Object is access all W_Object_Type'Class;
   package W_Object_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, W_Object);
   use W_Object_Maps;
   package W_Object_Vectors is new Ada.Containers.Vectors (Positive, W_Object);
   use W_Object_Vectors;

   type Data_Frame_Type;
   type Data_Frame is access all Data_Frame_Type;
   package Data_Frame_Vectors is new Ada.Containers.Vectors (Positive, Data_Frame);
   use Data_Frame_Vectors;

   type Frame_Context_Type;
   type Frame_Context is access all Frame_Context_Type;

   type Allocate_Callback is access procedure (E : access W_Object_Type'Class);

   --  A Frame_Context is a type that is recording stack-based properties that
   --  vary within a given frame, typically through an expression, or various
   --  parts of a command. Each Frame is supposed to start with a fresh frame
   --  context (ie information does not travel through frame contexts).
   type Frame_Context_Type is record
      Parent_Context : Frame_Context;

      Is_Matching_Context : Boolean := False;
      Is_Folding_Context : Boolean := False;

      --  When hitting a capture expression, the name is being stored here so
      --  that the capturing expression can update its value.
      Name_Captured : Unbounded_Text_Type;

      --  In when folding, browsing functions need to evaluate this expression
      --  upon all successful matches
      Folding_Expression : Template_Node;

      --  Callback used to record objects allocated through the new () function.
      --  This needs to be set in particular in browsing functions, in order to
      --  be able to capture things such as child (new ()).
      An_Allocate_Callback : Allocate_Callback := null;

      --  When set, this identifies the value at the left of the expression.
      --  For example, in A (V => @ & "something"), @ is the left value refering
      --  to V.
      Left_Value : W_Object;
   end record;

   type Data_Frame_Type is record
      Parent_Frame : Data_Frame;

      Symbols         : W_Object_Maps.Map;
      Matched_Groups  : W_Object_Vectors.Vector;
      Data_Stack      : W_Object_Vectors.Vector;
      Top_Context     : Frame_Context;
      Lexical_Scope   : T_Entity;

      Temp_Names      : Text_Maps.Map;
   end record;

   function Get_Visible_Symbol (A_Frame: Data_Frame_Type; Name : Text_Type) return W_Object;

   function Get_Module (A_Frame : Data_Frame_Type) return Semantic.Structure.T_Module;

   --  This is the root type of all values that are manipulated by expressions
   type W_Object_Type is tagged record
      null;
   end record;

   function Push_Value
     (An_Entity : access W_Object_Type;
      Name      : Text_Type) return Boolean is (False);

   --  Calling an entity means either doing an actual call if this entity
   --  refers to a function, or performing a comparison between the object and
   --  the provided parameters. In the second case, by convention, the result
   --  of the call (last object on the stack) is either Match_False, or
   --  Match_True (An_Entity).
   --  By default, this returns an error (the object is not made for being called).
   procedure Push_Call_Result
     (An_Entity : access W_Object_Type;
      Params    : Argument_List);

   type Browse_Mode is (Parent, Child_Depth, Child_Breadth, Next, Prev, Sibling, Template);

   --  TODO: This is probably actually needed only at the node level.
   function Traverse
     (An_Entity    : access W_Object_Type;
      A_Mode       : Browse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action)
      return Visit_Action;

   -- TODO this into push browse_Result? And see with the other push browse
   -- restult what should be changed
   procedure Evaluate_Bowse_Functions
     (An_Entity        : access W_Object_Type;
      A_Mode           : Browse_Mode;
      Match_Expression : Template_Node'Class) is null;

   function Browse_Entity
     (An_Entity : access W_Object_Type;
      Browsed : access W_Object_Type'Class;
      Match_Expression : Template_Node'Class;
      Result : out W_Object) return Visit_Action;

   procedure Push_Match_True (An_Entity : access W_Object_Type'Class);

   procedure Push_Match_False;

   --  This function resolves a runtime object into the String value. The result
   --  of this function varies over time - as the underlying object gets
   --  completed by various wrapping and weaving opertions. This should be
   --  called as late as possible in the process (unless explicitely requested
   --  through e.g. a string conversion). Keeping a reference to the runtime
   --  object is prefered. Note that this is directly linked to the actual
   --  semantics of the language, so should remain consistent with it.
   function To_String (Object : W_Object_Type) return Text_Type is ("");

   Null_Object : constant W_Object := new W_Object_Type;

   Match_False : constant W_Object := Null_Object;

   function Get_Object_For_Entity (An_Entity : access T_Entity_Type'Class) return W_Object;

end Wrapping.Runtime.Structure;
