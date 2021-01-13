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

with Libadalang.Common;        use Libadalang.Common;
with Libadalang.Analysis;      use Libadalang.Analysis;
with Libadalang.Introspection; use Libadalang.Introspection;

with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Runtime.Structure; use Wrapping.Runtime.Structure;

package Wrapping.Input.Ada is

   type W_Property_Type is tagged;
   type W_Property is access all W_Property_Type'Class;

   type W_Unhandled_Value_Type is tagged;
   type W_Unhandled_Value is access all W_Unhandled_Value_Type'Class;

   function Get_Property (Node : Ada_Node; Name : Text_Type) return W_Object;
   --  Returns the W_Object that models the property of the given name for the]
   --  ada node in parameter, null if not found.

   type W_Property_Type is new W_Object_Type with record
      Property_Node : Any_Member_Reference;
   end record;
   --  Holds a reference to a specific property to be called.

   overriding function Type_Name
     (Object : W_Property_Type) return Text_Type is ("property");
   --  See parent documentation

   overriding procedure Push_Call_Result
     (An_Entity : access W_Property_Type;
      Params    : T_Arg_Vectors.Vector);
   --  Calls the property with the parameters in copy and pushes the result on
   --  the stack.

   type W_Unhandled_Value_Type is new W_Object_Type with record
      Data : Any_Value_Type;
   end record;
   --  This type is used for types that we're not directly handling in UWrap
   --  but that can be used passing data from and to properties.

   overriding function Type_Name
     (Object : W_Unhandled_Value_Type) return Text_Type is
     ("unhandled property value");
   --  See parent documentation

end Wrapping.Input.Ada;
