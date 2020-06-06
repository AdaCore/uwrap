with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Regpat; use GNAT.Regpat;

with Langkit_Support.Diagnostics;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libtemplatelang.Common;
with Libtemplatelang.Analysis;

with Wrapping.Regex; use Wrapping.Regex;
with Wrapping.Semantic.Analysis; use Wrapping.Semantic.Analysis;
with Wrapping.Semantic.Structure; use Wrapping.Semantic.Structure;
with Wrapping.Utils; use Wrapping.Utils;
with Wrapping.Runtime.Functions; use Wrapping.Runtime.Functions;

package body Wrapping.Runtime.Analysis is

   procedure Handle_Identifier (Node : Libtemplatelang.Analysis.Template_Node'Class);
   procedure Handle_Call (Node : Libtemplatelang.Analysis.Template_Node'Class);
   procedure Handle_Traverse_Step (Node : Libtemplatelang.Analysis.Template_Node'Class);
   procedure Handle_Template_Call (An_Entity : Language_Entity; A_Template_Instance : Template_Instance; Args : Libtemplatelang.Analysis.Argument_List);

   procedure Analyze_Replace_String (Str : Text_Type; Context : Libtemplatelang.Analysis.Analysis_Context);
   function Visit_Expression (Node : Libtemplatelang.Analysis.Template_Node'Class) return Libtemplatelang.Common.Visit_Status;

   procedure Push_Error_Location
     (An_Entity : access Semantic.Structure.Entity_Type'Class)
   is
   begin
      Wrapping.Push_Error_Location
        (An_Entity.Unit.Get_Filename,
         (An_Entity.Sloc.Start_Line, An_Entity.Sloc.Start_Column));
   end Push_Error_Location;

   procedure Pop_Error_Location is
   begin
      Wrapping.Pop_Error_Location;
   end Pop_Error_Location;

   procedure Push_Entity (An_Entity : access Language_Entity_Type'Class; Is_Implicit_Self : Boolean := False) is
   begin
      Top_Frame.Data_Stack.Append (new Runtime_Language_Entity_Type'(Value => Language_Entity (An_Entity), Is_Implicit_Self => Is_Implicit_Self));
   end Push_Entity;

   procedure Push_Frame (Lexical_Scope : access Semantic.Structure.Entity_Type'Class) is
      New_Frame : Data_Frame := new Data_Frame_Type;
   begin
      New_Frame.Parent_Frame := Top_Frame;
      New_Frame.Lexical_Scope := Semantic.Structure.Entity (Lexical_Scope);

      Top_Frame := New_Frame;
   end Push_Frame;

   procedure Pop_Frame is
      procedure Free is new Ada.Unchecked_Deallocation (Data_Frame_Type, Data_Frame);

      Old_Frame : Data_Frame;
   begin
      Old_Frame := Top_Frame;
      Top_Frame := Old_Frame.Parent_Frame;

      Free (Old_Frame);
   end Pop_Frame;

   function Get_Self return Language_Entity is
   begin
      for I in reverse Top_Frame.Data_Stack.First_Index .. Top_Frame.Data_Stack.Last_Index loop
         if Top_Frame.Data_Stack.Element (I).all in Runtime_Language_Entity_Type
           and then Runtime_Language_Entity (Top_Frame.Data_Stack.Element (I)).Is_Implicit_Self
         then
            return Runtime_Language_Entity (Top_Frame.Data_Stack.Element (I)).Value;
         end if;
      end loop;

      return null;
   end Get_Self;

   function Match (Pattern, Text : Text_Type) return Boolean is
      Text_Str : String := To_String (Text);
      Matches : Match_Obj :=
        Match (Compile (To_String (Pattern)), Text_Str);
   begin
      if Wrapping.Regex.No_Match (Matches) then
         return False;
      end if;

      for I in 1 .. Matches.Matches.Element'Last loop
         declare
            Matched_Text : Text_Type :=
              To_Text
                (Text_Str
                   (Matches.Matches.Element (I).First .. Matches.Matches.Element (I).Last));
            Name : Text_Type := To_Text (Get_Capture_Name (Matches, I));
         begin
            Top_Frame.Matched_Groups.Append
              (new Runtime_Text_Type'
                 (Value => To_Unbounded_Text (Matched_Text)));

            if Name /= "" then
               Top_Frame.Symbols.Include
                 (Name,
                  new Runtime_Text_Type'
                    (Value => To_Unbounded_Text (Matched_Text)));
            end if;
         end;
      end loop;

      return True;
   end Match;

   procedure Apply_Wrapping_Program
     (A_Language_Entity : Language_Entity;
      Lexical_Scope     : access Semantic.Structure.Entity_Type'Class;
      A_Visit_Action    : in out Visit_Action)
   is
      Self_Element : Runtime_Language_Entity := new Runtime_Language_Entity_Type; -- TODO: We may not need to instantiate here

      procedure Create_And_Set_Template_Instance
        (A_Command  : Command;
         Expression : Libtemplatelang.Analysis.Template_Node'Class);
      --  This will create a template instance from the clause information,
      --  setting parameter expression and adding it to the relevant language
      --  object.

      procedure Create_And_Set_Template_Instance
        (A_Command : Command;
         Expression : Libtemplatelang.Analysis.Template_Node'Class)
      is
      begin
         Evaluate_Expression (Expression);
      end Create_And_Set_Template_Instance;

      procedure Apply_Command (A_Command : Command) is
         Matched : Boolean;
         Match_Result : Runtime_Object;
      begin
         --  The command is the enclosing scope for all of its clauses. It
         --  will in particular receive the matching groups and the temporary
         --  values that can be used consistently in the various clauses
         Push_Frame (A_Command);
         Push_Entity (A_Language_Entity, True);

         if not A_Command.Match_Expression.Is_Null then
            Top_Frame.Context := Match_Context;
            Evaluate_Expression (A_Command.Match_Expression);
            Top_Frame.Context := Generic_Context;

            Match_Result := Top_Frame.Data_Stack.Last_Element;
            Top_Frame.Data_Stack.Delete_Last;

            Matched := Match_Result /= Match_False;
         else
            Matched := True;
         end if;

         if Matched then
            if A_Command.Template_Clause /= null then
               -- HANDLE WEAVE OR WRAP

               declare
                  Entity_Target : Language_Entity;
                  A_Template_Instance : Template_Instance;
               begin
                  if not A_Command.Template_Clause.Target_Object.Is_Null then
                     Evaluate_Expression (A_Command.Template_Clause.Target_Object);
                     Entity_Target := Runtime_Language_Entity (Top_Frame.Data_Stack.Last_Element).Value;
                     Top_Frame.Data_Stack.Delete_Last;
                  else
                     Entity_Target := A_Language_Entity;
                  end if;

                  if A_Command.Template_Clause.Template_Reference /= null then
                     A_Template_Instance := Entity_Target.Get_Template_Instance
                       (A_Command.Template_Clause.Template_Reference);

                     if A_Command.Template_Clause.all in Weave_Type'Class
                       or else A_Template_Instance = null
                       or else A_Template_Instance.Is_Wrapping = False
                     then
                        if A_Template_Instance = null then
                           A_Template_Instance := Entity_Target.Create_Template_Instance
                             (A_Command.Template_Clause.Template_Reference);
                        end if;

                        if  A_Command.Template_Clause.all in Wrap_Type'Class then
                           A_Template_Instance.Is_Wrapping := True;
                        end if;
                     else
                        A_Template_Instance := null;
                     end if;
                  else
                     Evaluate_Expression (A_Command.Template_Clause.Template_Instance_Expression);

                     if Top_Frame.Data_Stack.Last_Element.all not in Runtime_Language_Entity_Type'Class
                       or else Runtime_Language_Entity (Top_Frame.Data_Stack.Last_Element).Value.all not in
                       Template_Instance_Type'Class
                     then
                        Error ("template instance not found");
                     end if;

                     A_Template_Instance :=  Template_Instance
                       (Runtime_Language_Entity (Top_Frame.Data_Stack.Last_Element).Value);
                     Top_Frame.Data_Stack.Delete_Last;
                  end if;

                  if A_Template_Instance /= null then
                      Handle_Template_Call
                          (Entity_Target,
                           A_Template_Instance,
                           A_Command.Template_Clause.Arguments);
                  end if;
               end;
            elsif not A_Command.Apply_Expression.Is_Null then
               -- TODO
               -- Create a frame, independent from the other ones
               -- Resolve parameters and put them on the new stack
               -- call the command
               null;
            elsif A_Command.Nested_Actions /= null then
               Apply_Wrapping_Program
                 (A_Language_Entity,
                  A_Command.Nested_Actions,
                  A_Visit_Action);
            elsif not A_Command.Traverse_Expression.Is_Null then
               if not A_Language_Entity.Traverse_Applied then
                  A_Language_Entity.Traverse_Applied := True;

                  A_Command.Traverse_Expression.Traverse (Visit_Expression'Access);

                  if Top_Frame.Data_Stack.Last_Element.all in Runtime_Traverse_Decision_Type'Class then
                     --  In this case, a decision was taken as to the continuation of the
                     --  iteration.

                     declare
                        A_Decision : Runtime_Traverse_Decision := Runtime_Traverse_Decision
                          (Top_Frame.Data_Stack.Last_Element);
                     begin
                        Top_Frame.Data_Stack.Delete_Last;

                        if A_Decision.A_Visit_Action = Over then
                           A_Visit_Action := Over;
                        else
                           if A_Decision.Into_Expression.Is_Null then
                              A_Visit_Action := Into;
                           else
                              Error ("traverse into functions not yet implemented");

                              A_Visit_Action := Over;
                           end if;
                        end if;
                     end;
                  end if;
               end if;
            end if;
         else
            if A_Command.Else_Actions /= null then
               if A_Command.Else_Actions.all in Command_Type'Class then
                  Apply_Command (Command (A_Command.Else_Actions));
               else
                  Apply_Wrapping_Program
                    (A_Language_Entity,
                     A_Command.Else_Actions,
                     A_Visit_Action);
               end if;
            end if;
         end if;

         Top_Frame.Data_Stack.Delete_Last;

         Pop_Frame;
      end Apply_Command;
   begin
      Push_Frame (Lexical_Scope);

      for Wrapping_Entity of reverse Lexical_Scope.Children_Ordered loop
         if Wrapping_Entity.all in Command_Type then
            Apply_Command (Command (Wrapping_Entity));
         elsif Wrapping_Entity.all in Module_Type'Class
           or else Wrapping_Entity.all in Namespace_Type'Class
         then
            Apply_Wrapping_Program
              (A_Language_Entity, Wrapping_Entity, A_Visit_Action);
         end if;
      end loop;

      Pop_Frame;
   end Apply_Wrapping_Program;

   procedure Register_Standard_Globals is
      Template_Class : Language_Entity_Class := new Template_Language_Entity_Class_Type;
   begin
      Language_Class_Registry.Insert ("template", Template_Class);
   end Register_Standard_Globals;

   procedure Analyse (Language : Text_Type) is
   begin
      Analyse (Get_Root_Language_Entity (Language));
   end Analyse;

   function Analyze_Visitor (E : access Language_Entity_Type'Class) return Visit_Action is
      A_Visit_Action : Visit_Action := Into;
   begin
      Apply_Wrapping_Program
        (Language_Entity (E),
         Wrapping.Semantic.Analysis.Root,
         A_Visit_Action);

      return A_Visit_Action;
   end Analyze_Visitor;

   procedure Analyse (Root_Entity : Language_Entity) is
      File_Template : Wrapping.Semantic.Structure.Template;
      Out_Template : Wrapping.Semantic.Structure.Template;
      A_Template_Instance : Template_Instance;
      Dummy_Action : Visit_Action;
   begin
      Register_Standard_Globals;

      Dummy_Action := Root_Entity.Traverse
        (Child_Depth, True, Analyze_Visitor'Access);

      Get_Root_Language_Entity ("template").Children_Ordered.Append
        (Get_Root_Language_Entity ("template_tmp").Children_Ordered);

      Get_Root_Language_Entity ("template_tmp").Children_Ordered.Clear;
      Get_Root_Language_Entity ("template_tmp").Children_Indexed.Clear;

      Dummy_Action := Get_Root_Language_Entity ("template").Traverse
        (Child_Depth, True, Analyze_Visitor'Access);

      File_Template := Wrapping.Semantic.Structure.Template
        (Resolve_Module_By_Name ("standard").
             Children_Indexed.Element ("file"));

      Out_Template := Wrapping.Semantic.Structure.Template
        (Resolve_Module_By_Name ("standard").
             Children_Indexed.Element ("out"));

      Get_Root_Language_Entity ("template").Children_Ordered.Append
        (Get_Root_Language_Entity ("template_tmp").Children_Ordered);

      Get_Root_Language_Entity ("template_tmp").Children_Ordered.Clear;
      Get_Root_Language_Entity ("template_tmp").Children_Indexed.Clear;

      for Created_Template of Get_Root_Language_Entity ("template").Children_Ordered loop
         A_Template_Instance := Template_Instance (Created_Template);

         if Instance_Of (A_Template_Instance.Template, File_Template) then
            declare
               Path_Object : Runtime_Object;
               Content_Object : Runtime_Object;
               Output_File : File_Type;
            begin
               Push_Frame (Root);

               if not A_Template_Instance.Push_Value ("path") then
                  Error ("'path' component not found in file template");
               end if;

               Path_Object := Top_Frame.Data_Stack.Last_Element;
               Top_Frame.Data_Stack.Delete_Last;

               if not A_Template_Instance.Push_Value ("content") then
                  Error ("'content' component not found in file template");
               end if;

               Content_Object := Top_Frame.Data_Stack.Last_Element;
               Top_Frame.Data_Stack.Delete_Last;

               Create
                 (Output_File,
                  Out_File,
                  To_String (Path_Object.To_Text));

               Put (Output_File, Content_Object.To_Text);

               Close (Output_File);

               Pop_Frame;
            end;
         elsif Instance_Of (A_Template_Instance.Template, Out_Template) then
            declare
               Content_Object : Runtime_Object;
            begin
               Push_Frame (Root);

               if not A_Template_Instance.Push_Value ("content") then
                  Error ("'content' component not found in file template");
               end if;

               Content_Object := Top_Frame.Data_Stack.Last_Element;
               Top_Frame.Data_Stack.Delete_Last;

               Put (Content_Object.To_Text);

               Pop_Frame;
            end;
         end if;
      end loop;
   end Analyse;

   procedure Evaluate_Expression
     (Node : Libtemplatelang.Analysis.Template_Node'Class)
   is
      -- TODO: Review the need for this confusing function that doesn't really traverse
      Dummy : Libtemplatelang.Common.Visit_Status := Visit_Expression (Node);
   begin
      null;
   end Evaluate_Expression;

   function Visit_Expression (Node : Libtemplatelang.Analysis.Template_Node'Class) return Libtemplatelang.Common.Visit_Status is
      use Libtemplatelang.Analysis;
      use Libtemplatelang.Common;
   begin
      Push_Error_Location (Node);

      case Node.Kind is
         when Template_Match_Capture =>
            --  This expression captures the result of the underlying
            --  expression and lets its value pass through.

            Node.As_Match_Capture.F_Expression.Traverse (Visit_Expression'Access);

            if Top_Frame.Data_Stack.Last_Element /= Match_False then
               Top_Frame.Symbols.Include
                 (Node.As_Match_Capture.F_Captured.Text,
                  Top_Frame.Data_Stack.Last_Element);
            end if;

            Pop_Error_Location;
            return Over;
         when Template_Dotted_Name =>
            declare
               Result : Runtime_Object;
            begin
               if not Node.As_Dotted_Name.F_Prefix.Is_Null then
                  Node.As_Dotted_Name.F_Prefix.Traverse (Visit_Expression'Access);
                  Node.As_Dotted_Name.F_Suffix.Traverse (Visit_Expression'Access);
                  Result := Top_Frame.Data_Stack.Last_Element;
                  Top_Frame.Data_Stack.Delete_Last (2);
                  Top_Frame.Data_Stack.Append (Result);
               else
                  Node.As_Dotted_Name.F_Suffix.Traverse (Visit_Expression'Access);
               end if;
            end;

            Pop_Error_Location;
            return Over;
         when Template_Selector =>
            declare
               Result : Runtime_Object;
            begin
               --  In a selector, we compute the left object, build the right
               --  expression based on the left object, and then put the result
               --  on the left object on the stack.

               Node.As_Selector.F_Left.Traverse (Visit_Expression'Access);
               Node.As_Selector.F_Right.Traverse (Visit_Expression'Access);
               Result := Top_Frame.Data_Stack.Last_Element;
               Top_Frame.Data_Stack.Delete_Last (2);
               Top_Frame.Data_Stack.Append (Result);
            end;

            Pop_Error_Location;
            return Over;
         when Template_Binary_Expr =>
            declare
               Left, Right : Runtime_Object;
            begin
               Node.As_Binary_Expr.F_Lhs.Traverse (Visit_Expression'Access);
               Left := Top_Frame.Data_Stack.Last_Element;
               Top_Frame.Data_Stack.Delete_Last;

               if Node.As_Binary_Expr.F_Op.Kind = Template_Operator_And then
                  if Left /= Match_False then
                     Node.As_Binary_Expr.F_Rhs.Traverse (Visit_Expression'Access);
                     Right := Top_Frame.Data_Stack.Last_Element;
                     Top_Frame.Data_Stack.Delete_Last;

                     if Right /= Match_False then
                        --  If this is a match, stack the current value for
                        --  check or capture later

                        Push_Match_True (Top_Frame.Data_Stack.Last_Element);
                     else
                        Push_Match_False;
                     end if;
                  else
                     Push_Match_False;
                  end if;
               elsif Node.As_Binary_Expr.F_Op.Kind = Template_Operator_Or then
                  if Left /= Match_False then
                     Push_Match_True (Top_Frame.Data_Stack.Last_Element);
                  else
                     Node.As_Binary_Expr.F_Rhs.Traverse (Visit_Expression'Access);
                     Right := Top_Frame.Data_Stack.Last_Element;
                     Top_Frame.Data_Stack.Delete_Last;

                     if Right /= Match_False then
                        Push_Match_True (Top_Frame.Data_Stack.Last_Element);
                     else
                        Push_Match_False;
                     end if;
                  end if;
               end if;
            end;

            Pop_Error_Location;
            return Over;
         when Template_Unary_Expr =>
            declare
               Left : Runtime_Object;
            begin
               Node.As_Unary_Expr.F_Rhs.Traverse (Visit_Expression'Access);
               Left := Top_Frame.Data_Stack.Last_Element;
               Top_Frame.Data_Stack.Delete_Last;

               if Node.As_Unary_Expr.F_Op.Kind = Template_Operator_Not then
                  if Left = Match_False then
                     Push_Match_True (Top_Frame.Data_Stack.Last_Element);
                  else
                     Push_Match_False;
                  end if;
               end if;
            end;

            Pop_Error_Location;
            return Over;

         when Template_Literal =>
            if Node.Text = "true" then
               Push_Match_True (Top_Frame.Data_Stack.Last_Element);
            elsif Node.Text = "false" then
               Push_Match_False;
            else
               Error ("unkown literal '" & Node.Text & "'");
            end if;

            Pop_Error_Location;
            return Over;

         when Template_Token_Template | Template_Identifier =>
            Handle_Identifier (Node);

            Pop_Error_Location;
            return Over;
         when Template_Number =>
            declare
               Val : Runtime_Integer := new Runtime_Integer_Type;
            begin
               Val.Value := Integer'Wide_Wide_Value (Node.Text);

               Top_Frame.Data_Stack.Append (Runtime_Object (Val));
            end;

            Pop_Error_Location;
            return Over;
         when Template_Str =>
            declare
               Content : Text_Type := Remove_Quotes (Node.Text);
            begin
               Analyze_Replace_String (Content, Node.Unit.Context);
            end;

            if Top_Frame.Context = Match_Context then
               declare
                  To_Match : Runtime_Object := Top_Frame.Data_Stack.Last_Element;
               begin
                  Top_Frame.Data_Stack.Delete_Last;

                  if not Get_Self.Push_Match_Result (To_Match, No_Argument_List) then
                     Push_Match_False;
                  end if;
               end;
            end if;

            Pop_Error_Location;
            return Over;
         when Template_Call_Expr =>
            --  Resolved the called identifier

            Handle_Call (Node);

            Pop_Error_Location;
            return Over;

         when Template_Traverse_Clause =>
            Pop_Error_Location;
            return Into;

         when Template_Traverse_Sequence =>
            Pop_Error_Location;
            return Into;

         when Template_Traverse_Step =>
            Handle_Traverse_Step (Node);

            Pop_Error_Location;
            return Over;

         when Template_Traverse_End_Into =>
            Top_Frame.Data_Stack.Append
              (new Runtime_Traverse_Decision_Type'
                 (A_Visit_Action => Into,
                  Into_Expression => Template_Node (Node.As_Traverse_End_Into.F_Call)));

            Pop_Error_Location;
            return Over;

         when Template_Traverse_End_Over =>
            Top_Frame.Data_Stack.Append
              (new Runtime_Traverse_Decision_Type'
                 (A_Visit_Action => Over, others => <>));

            Pop_Error_Location;
            return Over;

         when others =>
            Error ("unexpected expression node kind: '" & Node.Kind'Wide_Wide_Image & "'");
            Pop_Error_Location;
            return Into;
      end case;
   end Visit_Expression;

   ----------------------------
   -- Analyze_Replace_String --
   ----------------------------

   Expression_Unit_Number : Integer := 1;

   procedure Analyze_Replace_String (Str : Text_Type; Context : Libtemplatelang.Analysis.Analysis_Context) is
      use Libtemplatelang.Analysis;
      use Libtemplatelang.Common;
      use Langkit_Support.Diagnostics;

      Result : Runtime_Text_Container := new Runtime_Text_Container_Type;
      New_Text : Runtime_Text;

      Next_Index : Integer := Str'First;

      Current : Integer := Str'First;

      procedure Append_Text (Text : Text_Type) is
      begin
         New_Text := new Runtime_Text_Type;
         New_Text.Value := To_Unbounded_Text (Text);

         Result.Texts.Append (Runtime_Text_Expression (New_Text));
      end Append_Text;
   begin
      if Str = "" then
         Append_Text ("");
         return;
      end if;

      while Current <= Str'Last loop
         if Str (Current) = '\' then
            if Current /= Str'First then
               Append_Text (Str (Next_Index .. Current - 1));
            end if;

            Current := Current + 1;

            if Str (Current) = 'e' then
               Current := Current + 1;

               if Str (Current) = '<' then
                  Next_Index := Current;

                  while Next_Index < Str'Last and then Str (Next_Index) /= '>' loop
                     Next_Index := Next_Index + 1;
                  end loop;

                  Expression_Unit_Number := Expression_Unit_Number + 1;

                  declare
                     Expression_Unit : Analysis_Unit :=
                       Get_From_Buffer
                         (Context  => Context,
                          Filename => "internal expression" & Expression_Unit_Number'Img,
                          Buffer   => To_String (Str (Current + 1 .. Next_Index - 1)),
                          Rule     => Expr_Rule);
                  begin
                     if Has_Diagnostics (Expression_Unit) then
                        Error (To_Text (Diagnostics (Expression_Unit)(1).Message));
                     end if;

                     Evaluate_Expression (Expression_Unit.Root);

                     Result.Texts.Append (Top_Frame.Data_Stack.Last_Element.To_Text_Expression);

                     Top_Frame.Data_Stack.Delete_Last;
                  end;

                  Current := Next_Index + 1;
                  Next_Index := Current;
               else
                  Append_Text (Str (Current - 1 .. Current));
                  Next_Index := Current;
                  Current := Current + 1;
               end if;
            elsif Str (Current) = 'n' then
               Append_Text (To_Text (String'(1 => ASCII.LF)));
               Current := Current + 1;
               Next_Index := Current;
            elsif Str (Current) in '0' .. '9' then
               Next_Index := Current;

               while Next_Index < Str'Last and then Str (Next_Index) in '0' .. '9' loop
                  Next_Index := Next_Index + 1;
               end loop;

               if Str (Next_Index) not in '0' .. '9' then
                  Next_Index := Next_Index - 1;
               end if;

               Append_Text
                 (Top_Frame.Matched_Groups.Element
                    (Natural'Wide_Wide_Value (Str (Current .. Next_Index))).To_Text);

               Current := Next_Index + 1;
               Next_Index := Current;
            elsif Str (Current) = '\' then
               Append_Text ("\");
               Next_Index := Current;
               Current := Current + 1;
            else
               Current := Current + 1;
            end if;
         else
            Current := Current + 1;
         end if;
      end loop;

      if Next_Index <= Str'Last then
         -- Add the end of the text to the result

         Append_Text (Str (Next_Index .. Str'Last));
      end if;

      Top_Frame.Data_Stack.Append (Runtime_Object (Result));
   end Analyze_Replace_String;

   procedure Handle_Identifier (Node : Libtemplatelang.Analysis.Template_Node'Class) is
      Prefix_Entity : Language_Entity;
      Tentative_Symbol : Runtime_Object;
      A_Semantic_Entity : Semantic.Structure.Entity;

      procedure Handle_Global_Identifier is
         A_Module : Semantic.Structure.Module;
         Name : Text_Type := Node.Text;
      begin
         -- Check in the dynamic symols in the frame

         Tentative_Symbol := Get_Visible_Symbol (Top_Frame.all, Name);

         A_Module := Get_Module (Top_Frame.all);

         -- Check in the static symbols in the module

         if A_Module.Children_Indexed.Contains (Name) then
            if Tentative_Symbol = null then
               A_Semantic_Entity := A_Module.Children_Indexed (Name);

               if A_Semantic_Entity.all in Template_Type'Class then
                  Tentative_Symbol := new Runtime_Static_Entity_Type'
                    (An_Entity => A_Semantic_Entity);
               end if;
            else
               Error ("can't reference " & Node.Text & ", multiple definitions hiding");
            end if;
         end if;

          -- Check in the imported symbols in the module

         for Imported of A_Module.Imported_Modules loop
            if Imported.Children_Indexed.Contains (Name) then
               if Tentative_Symbol = null then
                  A_Semantic_Entity := Imported.Children_Indexed (Name);

                  if A_Semantic_Entity.all in Template_Type'Class then
                     Tentative_Symbol := new Runtime_Static_Entity_Type'
                       (An_Entity => A_Semantic_Entity);
                  end if;
               else
                  Error ("can't reference " & Node.Text & ", multiple definitions hiding");
               end if;
            end if;
         end loop;

         -- Check in the namesaces symbols

         if Root.Children_Indexed.Contains (Name) then
            if Tentative_Symbol = null then
               A_Semantic_Entity := Root.Children_Indexed.Element (Name);

               if A_Semantic_Entity.all in Namespace_Type'Class
                 or else A_Semantic_Entity.all in Module_Type'Class
               then
                  Tentative_Symbol := new Runtime_Static_Entity_Type'
                    (An_Entity => A_Semantic_Entity);
               end if;
            else
               Error ("can't reference " & Node.Text & ", multiple definitions hiding");
            end if;
         end if;

         if Tentative_Symbol = null then
            Error ("can't find global reference to '" & Name & "'");
         else
            Top_Frame.Data_Stack.Append (Tentative_Symbol);
         end if;
      end Handle_Global_Identifier;

      procedure Handle_Static_Entity_Selection is
         An_Entity : Semantic.Structure.Entity;
      begin
         An_Entity := Runtime_Static_Entity (Top_Frame.Data_Stack.Last_Element).An_Entity;

         if not An_Entity.Children_Indexed.Contains (Node.Text) then
            Error ("'" & Node.Text & "' not found");
         end if;

         A_Semantic_Entity := An_Entity.Children_Indexed.Element (Node.Text);

         Top_Frame.Data_Stack.Append
           (new Runtime_Static_Entity_Type'(An_Entity => A_Semantic_Entity));
      end Handle_Static_Entity_Selection;

      procedure Handle_Language_Entity_Selection is
         Name : Text_Type := Node.Text;
      begin
         -- We're resolving a reference to an entity

         Prefix_Entity := Runtime_Language_Entity (Top_Frame.Data_Stack.Last_Element).Value;

         if Runtime_Language_Entity
           (Top_Frame.Data_Stack.Last_Element).Is_Implicit_Self
           and then Name = "self"
         then
            --  We're just refering to self here, re-stack it without the implicit
            --  flag, it's now explicit
            Top_Frame.Data_Stack.Append
              (new Runtime_Language_Entity_Type'
                 (Value            => Prefix_Entity,
                  Is_Implicit_Self => False));

            return;
         end if;

         --  TODO: If there's already a template instance set, this should allow
         --  to retreive it (later call is only doing creation on the implicit
         --  self). Push Value should take that into account (it doesn't yet)
         if Prefix_Entity.Push_Value (Name) then
            --  We found a component of the entity and it has been pushed
            return;
         end if;

         if Runtime_Language_Entity
           (Top_Frame.Data_Stack.Last_Element).Is_Implicit_Self
         then
            --  We stacked the implicit self but didn't find a component, so
            --  we're actually not trying to select on the implicit self. Look
            --  for global references.
            Handle_Global_Identifier;
         else
            Error ("'" & Name & "' component not found");
         end if;
      end Handle_Language_Entity_Selection;

   begin
      if Top_Frame.Data_Stack.Length /= 0 then
         if Top_Frame.Data_Stack.Last_Element.all in Runtime_Static_Entity_Type'Class then
            Handle_Static_Entity_Selection;
         elsif Top_Frame.Data_Stack.Last_Element.all in Runtime_Language_Entity_Type then
            Handle_Language_Entity_Selection;
         else
            Handle_Global_Identifier;
         end if;
      else
         Handle_Global_Identifier;
      end if;
   end Handle_Identifier;

   procedure Handle_Template_Call
     (An_Entity : Language_Entity;
      A_Template_Instance : Template_Instance;
      Args : Libtemplatelang.Analysis.Argument_List)
   is
      Parameter_Index : Integer;
      Parameter_Value : Runtime_Object;
      In_Named_Section : Boolean := False;
      Name_Node : Libtemplatelang.Analysis.Template_Node;
   begin
      Parameter_Index := 1;

      for Param of Args loop
         Evaluate_Expression (Param.F_Value);

         Parameter_Value := Top_Frame.Data_Stack.Last_Element;
         Top_Frame.Data_Stack.Delete_Last;

         if not Param.As_Argument.F_Name.Is_Null then
            In_Named_Section := True;
            Name_Node := Param.As_Argument.F_Name;

            if not A_Template_Instance.Template.Has_Variable (Name_Node.Text) then
               Error ("no variable '" & Name_Node.Text & "' for template '" & Full_Name (A_Template_Instance.Template.all) & "'");
            end if;
         else
            if In_Named_Section then
               Error ("can't have positional arguments after named ones");
            end if;

            --
            Name_Node := A_Template_Instance.Template.Get_Variable_For_Index
              (Parameter_Index).Name_Node;
         end if;

         declare
            Container : Runtime_Text_Container;
         begin
            if A_Template_Instance.Symbols.Contains (Name_Node.Text) then
               Container := Runtime_Text_Container
                 (A_Template_Instance.Symbols.Element (Name_Node.Text));
            else
               Container := new Runtime_Text_Container_Type;
               A_Template_Instance.Symbols.Insert
                 (Name_Node.Text, Runtime_Object (Container));
            end if;

             Container.Texts.Append (Parameter_Value.To_Text_Expression);
         end;

         Parameter_Index := Parameter_Index + 1;
      end loop;
   end Handle_Template_Call;

   procedure Handle_Call (Node : Libtemplatelang.Analysis.Template_Node'Class) is
      use Libtemplatelang.Analysis;

      procedure Handle_Function_Call is
         A_Function : Runtime_Function_Reference :=
           Runtime_Function_Reference (Top_Frame.Data_Stack.Last_Element);
      begin
         Top_Frame.Data_Stack.Delete_Last;

         if not A_Function.Prefix.Push_Call_Result
           (To_Text (A_Function.Name), Node.As_Call_Expr.F_Args)
         then
            Error ("function '" & To_Text (A_Function.Name) & "' can't be called");
         end if;
      end Handle_Function_Call;

      procedure Handle_Match is
         Match_Object : Runtime_Object;
         Self : Language_Entity;
         Scope : Semantic.Structure.Entity;
         Selected : Semantic.Structure.Entity;
         A_Module : Semantic.Structure.Module;
      begin
         Match_Object := Top_Frame.Data_Stack.Last_Element;

         --  At this point, we stacked references to various namespaces. They
         --  will get unstacked when exiting the selectors. We do need to
         --  retreive the value of self earlier in the stack.
         Self := Get_Self;

         if Match_Object.all in Runtime_Language_Entity_Type'Class then
            if Runtime_Language_Entity (Match_Object).Value.Push_Match_Result
              (new Runtime_Field_Reference_Type'
                 (Name => To_Unbounded_Text (Node.As_Call_Expr.F_Called.Text)),
               Node.As_Call_Expr.F_Args)
            then
               return;
            end if;
         end if;

         A_Module := Get_Module (Top_Frame.all);

         if Match_Object.all in Runtime_Static_Entity_Type'Class then
            Scope := Runtime_Static_Entity (Match_Object).An_Entity;
         else
            Scope := Entity (A_Module);
         end if;

         if Scope.Children_Indexed.Contains (Node.As_Call_Expr.F_Called.Text) then
            Selected := Scope.Children_Indexed.Element (Node.As_Call_Expr.F_Called.Text);

            --  TODO: We might be able to go without a pointer in the signature
            --  of match result, avoiding dynamic memory allocation here.
            if Self.Push_Match_Result
              (new Runtime_Static_Entity_Type'(An_Entity => Selected),
               Node.As_Call_Expr.F_Args)
            then
               return;
            end if;
         end if;

         for Imported of A_Module.Imported_Modules loop
            if Imported.Children_Indexed.Contains (Node.As_Call_Expr.F_Called.Text) then

               Selected := Imported.Children_Indexed.Element (Node.As_Call_Expr.F_Called.Text);

               --  TODO: We might be able to go without a pointer in the signature
               --  of match result, avoiding dynamic memory allocation here.
               if Self.Push_Match_Result
                 (new Runtime_Static_Entity_Type'(An_Entity => Selected),
                  Node.As_Call_Expr.F_Args)
               then
                  return;
               end if;
            end if;
         end loop;

         Push_Match_False;
      end Handle_Match;

   begin
      if Top_Frame.Context = Match_Context then
         Handle_Match;
      else
         Node.As_Call_Expr.F_Called.Traverse (Visit_Expression'Access);

         if Top_Frame.Data_Stack.Last_Element.all in Runtime_Call_To_Global_Type'Class then
            Runtime_Call_To_Global_Type'Class (Top_Frame.Data_Stack.Last_Element.all).Analyze_Parameters (Node.As_Call_Expr.F_Args);
         elsif Top_Frame.Data_Stack.Last_Element.all in Runtime_Function_Reference_Type then
            Handle_Function_Call;
         else
            Error ("expected function call or template reference");
         end if;
      end if;
   end Handle_Call;

   procedure Handle_Traverse_Step (Node : Libtemplatelang.Analysis.Template_Node'Class) is
      Entity : Language_Entity;

      Dummy_Action : Visit_Action;
   begin
      Node.As_Traverse_Step.F_Expression.Traverse (Visit_Expression'Access);
      Entity := Runtime_Language_Entity (Top_Frame.Data_Stack.Last_Element).Value;

      if Node.As_Traverse_Step.F_Into.Is_Null then
         Dummy_Action := Entity.Traverse (Child_Depth, True, Analyze_Visitor'Access);
         Top_Frame.Data_Stack.Delete_Last;
      else
         Error ("traversing into functions not implemented");
      end if;

      if not Node.As_Traverse_Step.F_Then.Is_Null then
         Node.As_Traverse_Step.F_Then.Traverse (Visit_Expression'Access);
      end if;
   end Handle_Traverse_Step;

end Wrapping.Runtime.Analysis;
