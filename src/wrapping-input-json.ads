with Ada.Containers.Indefinite_Ordered_Maps;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Wrapping.Runtime.Objects; use Wrapping.Runtime.Objects;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;

package Wrapping.Input.JSON is

   type W_JSON_Node_Type;
   type W_JSON_Node is access all W_JSON_Node_Type'Class;

   type W_JSON_Node_Type is new W_Node_Type with record
      Node : JSON_Value;
      Name : Unbounded_Text_Type;
   end record;

   overriding
   function Push_Value
     (An_Entity : access W_JSON_Node_Type;
      Name      : Text_Type) return Boolean;

   overriding
   function To_String (Object : W_JSON_Node_Type) return Text_Type;

   overriding
   function To_Debug_String (Object : W_JSON_Node_Type) return Text_Type;

   overriding
   function Language (Object : W_JSON_Node_Type) return Text_Type is ("json");

   procedure Analyze_File (Filename : String);

end Wrapping.Input.JSON;
