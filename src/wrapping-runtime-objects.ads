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

   type W_Map_Type;
   type W_Map is access all W_Map_Type;

   type W_Integer_Type;
   type W_Integer is access all W_Integer_Type'Class;

   type W_Text_Expression_Type;
   type W_Text_Expression is access all W_Text_Expression_Type'Class;
   package W_Text_Expression_Vectors is new Ada.Containers.Vectors (Positive, W_Text_Expression);
   use W_Text_Expression_Vectors;

   type W_String_Type;
   type W_String is access all W_String_Type'Class;

   type W_Regexp_Type;
   type W_Regexp is access all W_Regexp_Type'Class;

   type W_Text_Conversion_Type;
   type W_Text_Conversion is access all W_Text_Conversion_Type'Class;

   type W_Text_Vector_Type;
   type W_Text_Vector is access all W_Text_Vector_Type'Class;
   package W_Text_Vector_Vectors is new Ada.Containers.Vectors (Positive, W_Text_Vector);
   use W_Text_Vector_Vectors;

   type W_Text_Reindent_Type;
   type W_Text_Reindent is access all W_Text_Reindent_Type'Class;

   type W_Function_Type;
   type W_Function is access all W_Function_Type'Class;

   type W_Static_Entity_Type;
   type W_Static_Entity is access all W_Static_Entity_Type'Class;

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

   type W_All_Type;
   type W_All is access all W_All_Type;

   type W_Reference_Type is new W_Object_Type with record
      Value : W_Object;

      Is_Implicit_Self : Boolean := False;

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
      Params    : T_Arg_Vectors.Vector);

   function Match_With_Top_Object
     (An_Entity : access W_Reference_Type) return Boolean;

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
      Match_Expression : T_Expr);

   overriding
   function Browse_Entity
     (An_Entity : access W_Reference_Type;
      Browsed : access W_Object_Type'Class;
      Match_Expression : T_Expr;
      Result : out W_Object) return Visit_Action;

   function Is_Implicit (Object : W_Reference_Type) return Boolean is
     (Object.Is_Implicit_Self);

   overriding
   function Dereference
     (Object : access W_Reference_Type)
      return W_Object is (Object.Value.Dereference);

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
   function Push_Value
     (An_Entity : access W_Vector_Type;
      Name      : Text_Type) return Boolean;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Vector_Type;
      Params    : T_Arg_Vectors.Vector);

   overriding
   function To_String (Object : W_Vector_Type) return Text_Type;

   type W_Set_Type is new W_Object_Type with record
      A_Set : W_Object_Sets.Set;
   end record;

   overriding
   function Push_Value
     (An_Entity : access W_Set_Type;
      Name      : Text_Type) return Boolean;

   type W_Map_Type is new W_Object_Type with record
      A_Map : W_Object_Any_Maps.Map;
   end record;

   overriding
   function Push_Value
     (An_Entity : access W_Map_Type;
      Name      : Text_Type) return Boolean;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Map_Type;
      Params    : T_Arg_Vectors.Vector);

   type W_Integer_Type is new W_Object_Type with record
      Value : Integer;
   end record;

   overriding
   function To_String (Object : W_Integer_Type) return Text_Type;

   type W_Text_Expression_Type is abstract new W_Object_Type with record
      null;
   end record;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Text_Expression_Type;
      Params    : T_Arg_Vectors.Vector);

   type W_String_Type is new W_Text_Expression_Type with record
      Value : Unbounded_Text_Type;
   end record;

   overriding
   function To_String (Object : W_String_Type) return Text_Type;

   overriding
   function Lt
     (Left : access W_String_Type; Right : access W_Object_Type'Class)
      return Boolean;

   overriding
   function Eq
     (Left : access W_String_Type; Right : access W_Object_Type'Class)
      return Boolean;

   type W_Regexp_Type is new W_Text_Expression_Type with record
      Value : W_Object;
   end record;

   overriding
   function To_String (Object : W_Regexp_Type) return Text_Type;

   --  This type is used to model an object that needs to converts a sub-object
   --  into string. This is useful to differenciate sitations where a piece
   --  of data has to be interpreted as a reference or as a text.
   type W_Text_Conversion_Type is new W_Text_Expression_Type with record
      An_Object : W_Object;
   end record;

   overriding
   function To_String (Object : W_Text_Conversion_Type) return Text_Type;

   type W_Text_Vector_Type is new W_Text_Expression_Type with record
      A_Vector : W_Object_Vectors.Vector;
   end record;

   overriding
   function To_String (Object : W_Text_Vector_Type) return Text_Type;

   type Call_Access is access procedure
     (Object : access W_Object_Type'Class;
      Params : T_Arg_Vectors.Vector);

   type W_Text_Reindent_Type is new W_Text_Expression_Type with record
      Indent : Integer;
      Content : W_Object;
   end record;

   overriding
   function To_String (Object : W_Text_Reindent_Type) return Text_Type;

   type W_Function_Type is new W_Object_Type with record
      Prefix : W_Object;
      Call   : Call_Access;
   end record;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Function_Type;
      Params    : T_Arg_Vectors.Vector);

   type W_Static_Entity_Type is new W_Object_Type with record
      An_Entity : T_Entity;
   end record;

   overriding
   function Push_Value
     (An_Entity : access W_Static_Entity_Type;
      Name      : Text_Type) return Boolean;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Static_Entity_Type;
      Params    : T_Arg_Vectors.Vector);

   type W_Expression_Type is new W_Object_Type with record
      Expression : Template_Node;
   end record;

   type W_Traverse_Decision_Type is new W_Object_Type with record
      A_Visit_Action : Visit_Action;
      Into_Expression : Template_Node;
   end record;

   --  A lambda is a function that will be evaluated as late as possible, during
   --  the To_String calls. The environment is captured at creation time
   --  to that the expression can be valuated later on.
   type W_Lambda_Type is new W_Object_Type with record
      Captured_Symbols : W_Object_Maps.Map;
      Expr             : T_Expr;
      Implicit_Self    : W_Node;
      Implicit_New     : W_Node;
      Lexical_Scope    : T_Entity;
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
      A_Template : T_Template) return W_Template_Instance;

   function Get_Template_Instance
     (An_Entity : access W_Node_Type'Class;
      Name      : Text_Type) return W_Template_Instance;

   function Get_Template_Instance
     (An_Entity  : access W_Node_Type'Class;
      A_Template : T_Template) return W_Template_Instance;

   overriding
   function Push_Value
     (An_Entity : access W_Node_Type;
      Name      : Text_Type) return Boolean;

   overriding
   procedure Push_Call_Result
     (An_Entity : access W_Node_Type;
      Params    : T_Arg_Vectors.Vector);

   overriding
   function Match_With_Top_Object
     (An_Entity : access W_Node_Type) return Boolean;

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

   procedure Evaluate_Generator_Regexp
     (An_Entity : access W_Node_Type;
      A_Mode    : Browse_Mode;
      Expr      : T_Expr);

   overriding
   procedure Evaluate_Bowse_Functions
     (An_Entity        : access W_Node_Type;
      A_Mode           : Browse_Mode;
      Match_Expression : T_Expr);

   function To_String (An_Entity : W_Node_Type) return Text_Type is ("");

   procedure Print (An_Entity : W_Node_Type; Indent : Text_Type := "");

   type W_Template_Instance_Type is new W_Node_Type with record
      Defining_Entity : T_Entity;

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
   function Match_With_Top_Object
     (An_Entity : access W_Template_Instance_Type) return Boolean;

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

   type W_All_Type is record
      Iterable : W_Object;
      Prefix, Suffix : Template_node;
   end record;

end Wrapping.Runtime.Objects;
