with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

with Wrapping.Utils; Use Wrapping.Utils;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;

package Wrapping.Runtime.Objects is

   --  These types are the types manipulated by the wrapping language expression.
   --  They are all prefixed by W_ to be able to easily distinguish them from
   --  other types (in particular the Ada types of the same names).

   --  TODO: Review usage of this type - many instances do not actually require
   --  it now that we've merged the concept of language entity and runtime
   --  object.
   type W_Reference_Type;
   type W_Reference is access all W_Reference_Type;
   package W_Reference_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, W_Reference);
   use W_Reference_Maps;

   type W_Vector_Type;
   type W_Vector is access all W_Vector_Type'Class;
   package W_Vector_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, W_Vector);
   use W_Vector_Maps;

   type W_Set_Type;
   type W_Set is access all W_Set_Type;

   type W_Integer_Type;
   type W_Integer is access all W_Integer_Type'Class;

   type W_Text_Expression_Type;
   type W_Text_Expression is access all W_Text_Expression_Type'Class;
   package W_Text_Expression_Vectors is new Ada.Containers.Vectors (Positive, W_Text_Expression);
   use W_Text_Expression_Vectors;

   type W_String_Type;
   type W_String is access all W_String_Type'Class;
   function "<" (Left, Right : W_String) return Boolean;
   function "=" (Left, Right : W_String) return Boolean;
   package W_String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (W_String);
   use W_String_Sets;

   type W_Text_Conversion_Type;
   type W_Text_Conversion is access all W_Text_Conversion_Type'Class;

   type W_Function_Reference_Type;
   type W_Function_Reference is access all W_Function_Reference_Type'Class;

   type W_Static_Entity_Reference_Type;
   type W_Static_Entity_Reference is access all W_Static_Entity_Reference_Type'Class;

   type W_Expression_Type;
   type W_Expression is access all W_Expression_Type'Class;

   type W_Traverse_Decision_Type;
   type W_Traverse_Decision is access all W_Traverse_Decision_Type'Class;

   type W_Lambda_Type;
   type W_Lambda is access all W_Lambda_Type'Class;

   type W_Node_Type;
   type W_Node is access all W_Node_Type'Class;
   package W_Node_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, W_Node);
   use W_Node_Maps;
   package W_Node_Vectors is new Ada.Containers.Vectors (Positive, W_Node);
   use W_Node_Vectors;

   type W_Template_Instance_Type;
   type W_Template_Instance is access all W_Template_Instance_Type;
   package W_Template_Instance_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, W_Template_Instance);
   use W_Template_Instance_Maps;
   package W_Template_Instance_Vectors is new Ada.Containers.Vectors (Positive, W_Template_Instance);
   use W_Template_Instance_Vectors;

   type W_Reference_Type is new W_Object_Type with record
      Value : W_Object;

      Is_Implicit_Self : Boolean := False;
      Is_Implicit_New: Boolean := False;

      --  When entities are created in the expressions through the new function,
      --  this flag is set to true, so that the entity can be capture by
      --  enclosing functions such as child or sibling.
      Is_Allocated : Boolean := False;
   end record;

   overriding
   function Push_Value
     (An_Entity : access W_Reference_Type;
      Name      : Text_Type) return Boolean is
     (An_Entity.Value.Push_Value (Name));

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Reference_Type;
      Params    : Libtemplatelang.Analysis.Argument_List);

   overriding
   function Traverse
     (An_Entity    : access W_Reference_Type;
      A_Mode       : Browse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action)
      return Visit_Action;

   overriding
   procedure Evaluate_Bowse_Functions
     (An_Entity        : access W_Reference_Type;
      A_Mode           : Browse_Mode;
      Match_Expression : Template_Node'Class);

   overriding
   function Browse_Entity
     (An_Entity : access W_Reference_Type;
      Browsed : access W_Object_Type'Class;
      Match_Expression : Template_Node'Class;
      Result : out W_Object) return Visit_Action;

   function Is_Implicit (Object : W_Reference_Type) return Boolean is
     (Object.Is_Implicit_Self or else Object.Is_Implicit_New);

   overriding
   function To_String (Object : W_Reference_Type) return Text_Type is
     (if Object.Value /= null then Object.Value.To_String else "");

   --  This type is record vector of runtime objects. In the context of
   --  templates, it is also used to provide one level of indirection between
   --  the variables and the actual objects (which may themselves be containers,
   --  such as the "text" type.
   type W_Vector_Type is new W_Object_Type with record
      A_Vector : W_Object_Vectors.Vector;
   end record;

   --  Checks that this container only contains derivatives of Runtime_Text_Expression_Type.
   function Is_Text_Container (Container : W_Vector_Type) return Boolean;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Vector_Type;
      Params    : Libtemplatelang.Analysis.Argument_List);

   overriding
   function To_String (Object : W_Vector_Type) return Text_Type;

   type W_Set_Type is new W_Object_Type with record
      --  For now, we only support string sets.
      A_Set : W_String_Sets.Set;
   end record;

   type W_Integer_Type is new W_Object_Type with record
      Value : Integer;
   end record;

   overriding
   function To_String (Object : W_Integer_Type) return Text_Type;

   type W_Text_Expression_Type is abstract new W_Object_Type with record
      null;
   end record;

   type W_String_Type is new W_Text_Expression_Type with record
      Value : Unbounded_Text_Type;
   end record;

   overriding
   function To_String (Object : W_String_Type) return Text_Type;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_String_Type;
      Params    : Libtemplatelang.Analysis.Argument_List);

   function "<" (Left, Right : W_String) return Boolean is
     (Left.Value < Right.Value);

   function "=" (Left, Right : W_String) return Boolean is
     (Left.Value = Right.Value);

   --  This type is used to model an object that needs to converts a sub-object
   --  into string. This is useful to differenciate sitations where a piece
   --  of data has to be interpreted as a reference or as a text.
   type W_Text_Conversion_Type is new W_Text_Expression_Type with record
      An_Object : W_Object;
   end record;

   overriding
   function To_String (Object : W_Text_Conversion_Type) return Text_Type;

   type Call_Access is access procedure
     (Object : access W_Object_Type'Class;
      Params : Libtemplatelang.Analysis.Argument_List);

   type W_Function_Reference_Type is new W_Object_Type with record
      Prefix : W_Object;
      Call   : Call_Access;
   end record;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Function_Reference_Type;
      Params    : Libtemplatelang.Analysis.Argument_List);

   type W_Static_Entity_Reference_Type is new W_Object_Type with record
      An_Entity : Semantic.Structure.Entity;
   end record;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Static_Entity_Reference_Type;
      Params    : Libtemplatelang.Analysis.Argument_List);

   type W_Expression_Type is new W_Object_Type with record
      Expression : Libtemplatelang.Analysis.Template_Node;
   end record;

   type W_Traverse_Decision_Type is new W_Object_Type with record
      A_Visit_Action : Visit_Action;
      Into_Expression : Template_Node;
   end record;

   type W_Lambda_Type is new W_Object_Type with record
      Captured_Symbols : W_Object_Maps.Map;
      Expression : Libtemplatelang.Analysis.Template_Node;
      Implicit_Self : W_Node;
      Implicit_New : W_Node;
      Lexical_Scope : Semantic.Structure.Entity;
      -- TODO: also add the temporary register
   end record;

   overriding
   function To_String (Object : W_Lambda_Type) return Text_Type;

   type W_Node_Type is new W_Object_Type with record
      Parent, Next, Prev : W_Node;

      Children_Ordered : W_Node_Vectors.Vector;
      Children_Indexed : W_Node_Maps.Map;

      Templates_By_Name : W_Template_Instance_Maps.Map;
      Templates_By_Full_Id : W_Template_Instance_Maps.Map;
      Templates_Ordered : W_Template_Instance_Vectors.Vector;

      Forbidden_Template_Names : Text_Sets.Set;

      --  This flag trackes wether or not a traverse decision (e.g. wrap over)
      --  has already been taken. Once one decision is taken, no other can
      --  overlap.
      Traverse_Decision_Taken : Boolean := False;

      Tmp_Counter : Integer := 0;

      --  When the entity enters a vistor, the id of that visitor get
      --  stacked. This allows to track wether a given entity has been visited
      --  by a visitor invocation only once. Since the invocations are ordered,
      --  ids not in used anymore can be popped, keeping this list small.
      Visited_Stack : Integer_Vector.Vector;
   end record;

   procedure Add_Child (Parent, Child : access W_Node_Type'Class);

   procedure Add_Child (Parent, Child : access W_Node_Type'Class; Name : Text_Type);

   function Create_Template_Instance
     (An_Entity : access W_Node_Type'Class;
      A_Template : Semantic.Structure.Template) return W_Template_Instance;

   function Get_Template_Instance
     (An_Entity : access W_Node_Type'Class;
      Name      : Text_Type) return W_Template_Instance;

   function Get_Template_Instance
     (An_Entity  : access W_Node_Type'Class;
      A_Template : Semantic.Structure.Template) return W_Template_Instance;

   overriding
   function Push_Value
     (An_Entity : access W_Node_Type;
      Name      : Text_Type) return Boolean;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Node_Type;
      Params    : Libtemplatelang.Analysis.Argument_List);

   procedure Pre_Visit (An_Entity : access W_Node_Type) is null;

   overriding
   function Traverse
     (An_Entity    : access W_Node_Type;
      A_Mode       : Browse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action)
      return Visit_Action;

   overriding
   procedure Evaluate_Bowse_Functions
     (An_Entity        : access W_Node_Type;
      A_Mode           : Browse_Mode;
      Match_Expression : Template_Node'Class);

   overriding
   function Browse_Entity
     (An_Entity : access W_Node_Type;
      Browsed : access W_Object_Type'Class;
      Match_Expression : Template_Node'Class;
      Result : out W_Object) return Visit_Action;

   function To_String (An_Entity : W_Node_Type) return Text_Type is ("");

   procedure Print (An_Entity : W_Node_Type; Indent : Text_Type := "");

   type W_Template_Instance_Type is new W_Node_Type with record
      Template : Semantic.Structure.Template;

      --  This is used to record the actual values for the template variables.
      --  There is always one level of indirection between the variable and
      --  its actual data, so that when the variable is modified, references to
      --  it are updated accordingly. For example, if you have something like:
      --     weave with (X => "somethign", Y => X);
      --  the reference to X is indirect, and if X changes through another
      --  weaver, it will also change Y. The only time where the symbol is
      --  automatically dereferenced is when using it as a left value reference,
      --  such as:
      --      weave with (X => @ & "somethign");
      --  which would otherwise create cycles and defeat the intended semantic.
      Symbols : W_Reference_Maps.Map;

      Origin : W_Node;

      Is_Wrapping : Boolean := False;
   end record;

   overriding
   function Push_Value
     (An_Entity : access W_Template_Instance_Type;
      Name      : Text_Type) return Boolean;

   overriding
   function Traverse
     (An_Entity    : access W_Template_Instance_Type;
      A_Mode       : Browse_Mode;
      Include_Self : Boolean;
      Final_Result : out W_Object;
      Visitor      : access function
        (E      : access W_Object_Type'Class;
         Result : out W_Object) return Visit_Action)
      return Visit_Action;

end Wrapping.Runtime.Objects;