with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with Langkit_Support.Text; use Langkit_Support.Text;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Utils; use Wrapping.Utils;
with Libtemplatelang.Analysis; use Libtemplatelang.Analysis;

package Wrapping.Runtime.Structure is
   -- The purpose is to provide the data structures created live during the
   -- wrapping process (e.g. call stacks, template instances, etc.)

   type Data_Frame_Type;
   type Data_Frame is access all Data_Frame_Type;
   package Data_Frame_Vectors is new Ada.Containers.Vectors (Positive, Data_Frame);
   use Data_Frame_Vectors;

   type Language_Entity_Class_Type;
   type Language_Entity_Class is access all Language_Entity_Class_Type'Class;
   package Language_Entity_Class_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Language_Entity_Class);
   use Language_Entity_Class_Maps;
   package Language_Entity_Class_Vectors is new Ada.Containers.Vectors (Positive, Language_Entity_Class);
   use Language_Entity_Class_Vectors;

   type Language_Entity_Type;
   type Language_Entity is access all Language_Entity_Type'Class;
   package Language_Entity_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Language_Entity);
   use Language_Entity_Maps;
   package Language_Entity_Vectors is new Ada.Containers.Vectors (Positive, Language_Entity);
   use Language_Entity_Vectors;

   type Template_Instance_Type;
   type Template_Instance is access all Template_Instance_Type;
   package Template_Instance_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Template_Instance);
   use Template_Instance_Maps;
   package Template_Instance_Vectors is new Ada.Containers.Vectors (Positive, Template_Instance);
   use Template_Instance_Vectors;

   type String_Langage_Entity_Type;
   type String_Langage_Entity is access all String_Langage_Entity_Type;

   type Post_Analyze_Action_Type;
   type Post_Analyze_Action is access all Post_Analyze_Action_Type;
   package Post_Analyze_Action_Vectors is new Ada.Containers.Vectors (Positive, Post_Analyze_Action);
   use Post_Analyze_Action_Vectors;

   type Runtime_Object_Type;
   type Runtime_Object is access all Runtime_Object_Type'Class;
   package Runtime_Object_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Runtime_Object);
   use Runtime_Object_Maps;
   package Runtime_Object_Vectors is new Ada.Containers.Vectors (Positive, Runtime_Object);
   use Runtime_Object_Vectors;

   type Runtime_Integer_Type;
   type Runtime_Integer is access all Runtime_Integer_Type'Class;

   type Runtime_Text_Expression_Type;
   type Runtime_Text_Expression is access all Runtime_Text_Expression_Type'Class;

   package Runtime_Text_Expression_Vectors is new Ada.Containers.Vectors (Positive, Runtime_Text_Expression);
   use Runtime_Text_Expression_Vectors;

   type Runtime_Text_Container_Type;
   type Runtime_Text_Container is access all Runtime_Text_Container_Type'Class;

   type Runtime_Text_Type;
   type Runtime_Text is access all Runtime_Text_Type'Class;

   type Runtime_Language_Entity_Type;
   type Runtime_Language_Entity is access all Runtime_Language_Entity_Type'Class;

   type Runtime_Function_Reference_Type;
   type Runtime_Function_Reference is access all Runtime_Function_Reference_Type'Class;

   type Runtime_Static_Entity_Type;
   type Runtime_Static_Entity is access all Runtime_Static_Entity_Type'Class;

   type Runtime_Field_Reference_Type;
   type Runtime_Field_Reference is access all Runtime_Field_Reference_Type'Class;

   type Runtime_Expression_Type;
   type Runtime_Expression is access all Runtime_Expression_Type'Class;

   type Runtime_Traverse_Decision_Type;
   type Runtime_Traverse_Decision is access all Runtime_Traverse_Decision_Type'Class;

   type Runtime_Lambda_Type;
   type Runtime_Lambda is access all Runtime_Lambda_Type'Class;

   type Frame_Context is
     (Generic_Context,
      Match_Context);

   package Text_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Text_Type, Text_Type);

   type Allocate_Callback is access procedure (E : access Language_Entity_Type'Class);

   type Data_Frame_Type is record
      Parent_Frame : Data_Frame;

      Symbols         : Runtime_Object_Maps.Map;
      Matched_Groups  : Runtime_Object_Vectors.Vector;
      Data_Stack      : Runtime_Object_Vectors.Vector;
      Context         : Frame_Context := Generic_Context;
      An_Allocate_Callback : Allocate_Callback := null;
      Lexical_Scope   : Semantic.Structure.Entity;

      Temp_Names      : Text_Maps.Map;
   end record;

   function Get_Visible_Symbol (A_Frame: Data_Frame_Type; Name : Text_Type) return Runtime_Object;

   function Get_Module (A_Frame : Data_Frame_Type) return Semantic.Structure.Module;

   type Language_Entity_Class_Type is tagged record
      null;
   end record;

   type Language_Entity_Type is tagged record
      Parent, Next, Prev : Language_Entity;

      Children_Ordered : Language_Entity_Vectors.Vector;
      Children_Indexed : Language_Entity_Maps.Map;

      Templates_By_Name : Template_Instance_Maps.Map;
      Templates_By_Full_Id : Template_Instance_Maps.Map;
      Templates_Ordered : Template_Instance_Vectors.Vector;

      A_Class : Language_Entity_Class; -- TODO: Probably get rid of this

      Traverse_Applied : Boolean := False;

      Tmp_Counter : Integer := 0;
   end record;

   procedure Add_Child (Parent, Child : access Language_Entity_Type'Class);

   procedure Add_Child (Parent, Child : access Language_Entity_Type'Class; Name : Text_Type);

   function Create_Template_Instance
     (An_Entity : access Language_Entity_Type'Class;
      A_Template : Semantic.Structure.Template) return Template_Instance;

   function Get_Template_Instance
     (An_Entity : access Language_Entity_Type'Class;
      Name      : Text_Type) return Template_Instance;

   function Get_Template_Instance
     (An_Entity  : access Language_Entity_Type'Class;
      A_Template : Semantic.Structure.Template) return Template_Instance;

   function Push_Value
     (An_Entity : access Language_Entity_Type;
      Name      : Text_Type) return Boolean;

   function Push_Call_Result
     (An_Entity : access Language_Entity_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   function Push_Match_Result
     (An_Entity : access Language_Entity_Type;
      Selector  : Runtime_Object;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   type Browse_Mode is (Parent, Child_Depth, Child_Breadth, Next, Prev, Sibling, Template);

   procedure Pre_Visit (An_Entity : access Language_Entity_Type) is null;

   function Traverse
     (An_Entity    : access Language_Entity_Type;
      A_Mode       : Browse_Mode;
      Include_Self : Boolean;
      Visitor      : access function (E : access Language_Entity_Type'Class) return Visit_Action)
      return Visit_Action;

   procedure Evaluate_Bowse_Functions
     (An_Entity                 : access Language_Entity_Type;
      A_Mode                    : Browse_Mode;
      Match_Expression          : Template_Node'Class;
      Match_Visitor    : access function (E : access Language_Entity_Type'Class) return Visit_Action := null
      -- This override the standard expression matching. If not null, the
      -- expected ABI is:
      --   (1) the entity under test is stacked as input
      --   (2) the result of the matching is added to the stack as output
     );

   procedure Push_Match_True (An_Entity : access Language_Entity_Type);

   procedure Push_Match_True (An_Entity : access Runtime_Object_Type'Class);

   procedure Push_Match_False;

   function To_Text (An_Entity : Language_Entity_Type) return Text_Type is ("");

   procedure Print (An_Entity : Language_Entity; Indent : Text_Type := "");

   type Template_Language_Entity_Class_Type is new Language_Entity_Class_Type with record
      null;
   end record;

   type Template_Instance_Type is new Language_Entity_Type with record
      Template : Semantic.Structure.Template;

      Symbols : Runtime_Object_Maps.Map;

      Origin : Language_Entity;

      Is_Wrapping : Boolean := False;
   end record;

   overriding
   function Push_Value
     (An_Entity : access Template_Instance_Type;
      Name      : Text_Type) return Boolean;

   overriding
   function Push_Call_Result
     (An_Entity : access Template_Instance_Type;
      Name      : Text_Type;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   overriding
   function Push_Match_Result
     (An_Entity : access Template_Instance_Type;
      Selector  : Runtime_Object;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   overriding
   procedure Evaluate_Bowse_Functions
     (An_Entity                 : access Template_Instance_Type;
      A_Mode                    : Browse_Mode;
      Match_Expression          : Template_Node'Class;
      Match_Visitor    : access function (E : access Language_Entity_Type'Class) return Visit_Action := null);

   --  This type can be used for example when there's a string comparison to
   --  prepare in a match context. See the behavior of strings in the
   --  expression evaluator.
   type String_Langage_Entity_Type is new Language_Entity_Type with record
      Value : Unbounded_Text_Type;
   end record;

   overriding
   function Push_Match_Result
     (An_Entity : access String_Langage_Entity_Type;
      Selector  : Runtime_Object;
      Params    : Libtemplatelang.Analysis.Argument_List) return Boolean;

   type Post_Analyze_Action_Kind is (Add_Child, Add_Sibling, Add_Prev, Add_Next);

   type Post_Analyze_Action_Type (Kind : Post_Analyze_Action_Kind := Add_Child) is record
      From : Template_Node;

      case Kind is
         when Add_Child | Add_Sibling | Add_Prev | Add_Next =>
            Base_Entity : Language_Entity;
            New_Entity : Language_Entity;

      end case;
   end record;

   procedure Perform (Action : Post_Analyze_Action_Type);

   type Runtime_Object_Type is tagged record
      null;
   end record;

   function To_Text (Object : Runtime_Object_Type) return Text_Type is ("");

   --  TODO: check that we still need this text expression, we may just be
   --  carrying text bits all over. The only place where this might be
   --  useful is when computing temporary identifiers
   function To_Text_Expression (Object : access Runtime_Object_Type) return Runtime_Text_Expression is (null);

   Null_Runtime_Object : constant Runtime_Object := new Runtime_Object_Type;

   Match_False : constant Runtime_Object := Null_Runtime_Object;

   type Runtime_Integer_Type is new Runtime_Object_Type with record
      Value : Integer;
   end record;

   overriding
   function To_Text (Object : Runtime_Integer_Type) return Text_Type;

   overriding
   function To_Text_Expression (Object : access Runtime_Integer_Type) return Runtime_Text_Expression;

   type Runtime_Text_Expression_Type is abstract new Runtime_Object_Type with record
      null;
   end record;

   type Runtime_Text_Container_Type is new Runtime_Text_Expression_Type with record
      Texts : Runtime_Text_Expression_Vectors.Vector;
   end record;

   overriding
   function To_Text (Object : Runtime_Text_Container_Type) return Text_Type;

   overriding
   function To_Text_Expression (Object : access Runtime_Text_Container_Type) return Runtime_Text_Expression;

   type Runtime_Text_Type is new Runtime_Text_Expression_Type with record
      Value : Unbounded_Text_Type;
   end record;

   overriding
   function To_Text (Object : Runtime_Text_Type) return Text_Type;

   overriding
   function To_Text_Expression (Object : access Runtime_Text_Type) return Runtime_Text_Expression;

   type Runtime_Language_Entity_Type is new Runtime_Object_Type with record
      Value : Language_Entity;

      Is_Implicit_Self : Boolean := False;
      Is_Implicit_New: Boolean := False;

      --  When entities are created in the expressions through the new function,
      --  this flag is set to true, so that the entity can be capture by
      --  enclosing functions such as child or sibling.
      Is_Allocated : Boolean := False;
   end record;

   function Is_Implicit (Object : Runtime_Language_Entity_Type) return Boolean is
      (Object.Is_Implicit_Self or else Object.Is_Implicit_New);

   overriding
   function To_Text (Object : Runtime_Language_Entity_Type) return Text_Type is
     (if Object.Value /= null then Object.Value.To_Text else "");

   overriding
   function To_Text_Expression (Object : access Runtime_Language_Entity_Type) return Runtime_Text_Expression
   is (new Runtime_Text_Type'(Value => To_Unbounded_Text (Object.To_Text)));

   type Runtime_Function_Reference_Type is new Runtime_Object_Type with record
      Name : Unbounded_Text_Type;
      Prefix : Language_Entity;
   end record;

   type Runtime_Static_Entity_Type is new Runtime_Object_Type with record
      An_Entity : Semantic.Structure.Entity;
   end record;

   type Runtime_Field_Reference_Type is new Runtime_Object_Type with record
      Name : Unbounded_Text_Type;
   end record;

   overriding
   function To_Text (Object : Runtime_Field_Reference_Type) return Text_Type is
      (To_Text (Object.Name));

   type Runtime_Expression_Type is new Runtime_Object_Type with record
      Expression : Libtemplatelang.Analysis.Template_Node;
   end record;

   type Runtime_Traverse_Decision_Type is new Runtime_Object_Type with record
      A_Visit_Action : Visit_Action;
      Into_Expression : Template_Node;
   end record;

   type Runtime_Lambda_Type is new Runtime_Text_Expression_Type with record
      Captured_Symbols : Runtime_Object_Maps.Map;
      Expression : Libtemplatelang.Analysis.Template_Node;
      Implicit_Self : Language_Entity;
      Implicit_New : Language_Entity;
      Lexical_Scope : Semantic.Structure.Entity;
      -- TODO: also add the temporary register
   end record;

   overriding
   function To_Text (Object : Runtime_Lambda_Type) return Text_Type;

   overriding
   function To_Text_Expression (Object : access Runtime_Lambda_Type) return Runtime_Text_Expression
   is (Runtime_Text_Expression (Object));

end Wrapping.Runtime.Structure;
