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

--  This package implements a generic version of an input tree for any langkit
--  generated library.

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;

with Wrapping.Runtime.Structure;  use Wrapping.Runtime.Structure;
with Wrapping.Runtime.Nodes;      use Wrapping.Runtime.Nodes;
with Wrapping.Runtime.Strings;    use Wrapping.Runtime.Strings;

generic
   Language_Name : Text_Type;

   type Kit_Node is tagged private;
   type Kit_Unit is tagged private;
   type Kit_Node_Array is array (Positive range <>) of Kit_Node;
   type Any_Member_Reference is (<>);
   type Any_Node_Type_Id is (<>);
   type Kit_Node_Kind_Type is (<>);
   type Analysis_Unit is tagged private;
   type Analysis_Context is tagged private;
   type Grammar_Rule is (<>);
   type Unit_Provider_Reference is private;
   type Token_Reference is private;
   type Token_Data_Type is private;
   type Token_Kind is (<>);
   type Token_Index is range <>;

   None : Any_Member_Reference;
   Default_Grammar_Rule : Grammar_Rule;
   Default_Charset : String;
   No_Unit_Provider_Reference : Unit_Provider_Reference;
   No_Node_Type_Id : Any_Node_Type_Id;

   No_Token : Token_Reference;

   with function Get_Property
     (Node : Kit_Node; Name : Text_Type) return W_Object;
   with function Children (Node : Kit_Node'Class) return Kit_Node_Array is <>;
   with function Parent (Node : Kit_Node'Class) return Kit_Node is <>;
   with function Hash (Node : Kit_Node) return Ada.Containers.Hash_Type is <>;
   with function Lookup_Member
     (Id : Any_Node_Type_Id; Name : Text_Type)
      return Any_Member_Reference is <>;
   with function Id_For_Kind
     (Kind : Kit_Node_Kind_Type) return Any_Node_Type_Id is <>;
   with function Eval_Syntax_Field
     (Node : Kit_Node'Class; Field : Any_Member_Reference)
      return Kit_Node is <>;
   with function Kind (Node : Kit_Node'Class) return Kit_Node_Kind_Type is <>;
   with function Is_Null (Node : Kit_Node'Class) return Boolean is <>;
   with function Kind_Name (Node : Kit_Node'Class) return String is <>;
   with function Text (Node : Kit_Node'Class) return Text_Type is <>;
   with function Create_Context
     (Charset       : String                  := Default_Charset;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      With_Trivia   : Boolean := True; Tab_Stop : Positive := 8)
      return Analysis_Context is <>;
   with function Get_From_File
     (Context : Analysis_Context'Class; Filename : String;
      Charset : String       := ""; Reparse : Boolean := False;
      Rule : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is <>;
   with function Has_Diagnostics
     (Unit : Analysis_Unit'Class) return Boolean is <>;
   with function Diagnostics
     (Unit : Analysis_Unit'Class) return Diagnostics_Array is <>;
   with function Root (Unit : Analysis_Unit'Class) return Kit_Node is <>;
   with function Lookup_DSL_Name
     (Name : Text_Type) return Any_Node_Type_Id is <>;
   with function Is_Derived_From
     (Id, Parent : Any_Node_Type_Id) return Boolean is <>;
   with function Full_Sloc_Image
     (Node : Kit_Node'Class) return Text_Type is <>;
   with function Sloc_Range
     (Node : Kit_Node'Class) return Source_Location_Range is <>;
   with function Get_Filename (Unit : Kit_Unit'Class) return String is <>;
   with function Unit (Node : Kit_Node'Class) return Kit_Unit is <>;
   with function Token_Start
     (Node : Kit_Node'Class) return Token_Reference is <>;
   with function Token_End
     (Node : Kit_Node'Class) return Token_Reference is <>;
   with function Next
     (Token : Token_Reference; Exclude_Trivia : Boolean := False)
      return Token_Reference is <>;
   with function Data (Token : Token_Reference) return Token_Data_Type is <>;
   with function Text (Token : Token_Reference) return Text_Type is <>;
   with function Kind (Token_Data : Token_Data_Type) return Token_Kind is <>;
   with function Is_Trivia (Token : Token_Reference) return Boolean is <>;
   with function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range is <>;
   with function First_Token
     (Unit : Kit_Unit'Class) return Token_Reference is <>;
   with function Index (Token : Token_Reference) return Token_Index is <>;
package Wrapping.Input.Kit is

   type W_Kit_Node_Type is tagged;
   type W_Kit_Node is access all W_Kit_Node_Type'Class;

   type W_Kit_Node_Token_Type is tagged;
   type W_Kit_Node_Token is access all W_Kit_Node_Token_Type'Class;
   package W_Kit_Node_Vectors is new Ada.Containers.Vectors
     (Token_Index, W_Kit_Node_Token);
   type W_Kit_Node_Vector_Access is access all W_Kit_Node_Vectors.Vector;

   procedure Analyze_File (File : String);
   --  Takes the file in parameter, parse the contents and run the uwrap
   --  program on it. Deferred commands still need to be analyzed, presumably
   --  once all files are analyzed. Only Kit_Node will be parsed. Tokens will
   --  only be retreived and analyzed by a token () query.

   procedure Analyze_Unit (Unit : Analysis_Unit);
   --  Same as Analyze_File but works directly on a Unit instead.

   ----------------
   -- W_Kit_Node --
   ----------------

   function Lt (Left, Right : Kit_Node) return Boolean;
   --  Sorts nodes by kind then location

   function Eq (Left, Right : W_Kit_Node) return Boolean;
   --  Returns true if both references are pointing to the same object

   package W_Kit_Node_Entity_Node_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Kit_Node, W_Kit_Node, Lt, Eq);
   use W_Kit_Node_Entity_Node_Maps;

   type W_Kit_Node_Type is new W_Node_Type with record
      Node              : Kit_Node;
      --  Pointer to the initial langkit node

      Children_Computed : Boolean := False;
      --  In order to avoid unecessary processing, children are computed
      --  on-demand by the Pre_Visit primitive. This flag is false up until
      --  this is done.

      Children_By_Node  : W_Kit_Node_Entity_Node_Maps.Map;
      --  Mapping between the child of this node and the original langkit node

      Tokens            : W_Kit_Node_Vector_Access;
      --  List of non-trivia tokens corresponding to the whole unit containing
      --  this node (shared by all nodes of the same unit)

      Trivia_Tokens     : W_Kit_Node_Vector_Access;
      --  List of trivia tokens corresponding to the whole unit containing
      --  this node (shared by all nodes of the same unit)
   end record;
   --  This type manages a reference to a node coming from langkit.

   overriding procedure Pre_Visit (An_Entity : access W_Kit_Node_Type);
   --  Computes children of the current node

   overriding function Push_Value
     (An_Entity : access W_Kit_Node_Type; Name : Text_Type) return Boolean;
   --  Pushes values for various intrinsic, fields and properties of the
   --  current node. Will also push the current entity if the entity is an
   --  instance of a type of the name in parameter.

   overriding function Write_String
     (Object : W_Kit_Node_Type) return Buffer_Slice;
   --  Write the entire code corresponding to that entity

   overriding function To_Debug_String
     (Object : W_Kit_Node_Type) return Text_Type;
   --  See parent documentation

   overriding function Language
     (An_Entity : W_Kit_Node_Type) return Text_Type is
     (Language_Name);
   --  See parent documentation

   function Get_Entity_For_Node (Node : Kit_Node'Class) return W_Kit_Node;
   --  Returns the unique W_Kit_Node corresponding to the unique node in
   --  parameter, creates one if none.

   ----------------------
   -- W_Kit_Token_Node --
   ----------------------

   type W_Kit_Node_Token_Type is new W_Node_Type with record
      Node : Token_Reference;
   end record;
   --  Holds a refernece to the token

   overriding function Push_Value
     (An_Entity : access W_Kit_Node_Token_Type; Name : Text_Type)
      return Boolean;
   --  Pushes various intrinsic of tokens, in particular information about
   --  location

   overriding function Write_String
     (Object : W_Kit_Node_Token_Type) return Buffer_Slice;
   --  Write the content of that particular token on the buffer

   overriding function To_Debug_String
     (Object : W_Kit_Node_Token_Type) return Text_Type;
   --  See parent documentation

   overriding function Language
     (An_Entity : W_Kit_Node_Token_Type) return Text_Type is
     (Language_Name & "_token");
   --  See parent documentation

   -------------------
   -- W_Source_Node --
   -------------------

   type W_Source_Node_Type is tagged;
   type W_Source_Node is access all W_Source_Node_Type'Class;

   type W_Source_Node_Type is new W_Object_Type with record
      A_Node : Kit_Node;
   end record;

   overriding function Write_String
     (Object : W_Source_Node_Type) return Buffer_Slice is
     (Write_String (Object.A_Node.Text));

end Wrapping.Input.Kit;
