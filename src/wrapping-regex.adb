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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

package body Wrapping.Regex is

   -------------
   -- Compile --
   -------------

   function Compile (Pattern : String) return Basic_Regex is
      Ret      : Basic_Regex;
      Proc_Str : Unbounded_String := To_Unbounded_String (Pattern);

      procedure Process_Pattern_Groups;

      ----------------------------
      -- Process_Pattern_Groups --
      ----------------------------

      procedure Process_Pattern_Groups is

         Cg_Str       : constant String          := "(\(.+?\))";
         Real_Re      : constant Pattern_Matcher := Compile (Cg_Str);
         Real_Cg : constant Match_Count     := Paren_Count (Regexp => Real_Re);
         Real_Matches : Match_Array (0 .. Real_Cg);

         Data_Str     : constant String := To_String (Proc_Str);
         Real_Index   : Natural         := 1;
         Real_Current : Natural         := Data_Str'First;

         procedure Find_Capture_Group;

         ------------------------
         -- Find_Capture_Group --
         ------------------------

         procedure Find_Capture_Group is
         begin
            for I of Ret.Names loop
               if I.Location.First = Real_Matches (0).First then
                  I.Location.Last := Real_Matches (0).Last;
                  I.Index         := Real_Index;
                  return;
               end if;
            end loop;
            --  if we get here, this match doesn't have a name
            Ret.Names.Append
              (Capture_Group'
                 (Name     => Null_Unbounded_String, Index => Real_Index,
                  Location => Real_Matches (0)));
         end Find_Capture_Group;
      begin
         loop
            GNAT.Regpat.Match
              (Self => Real_Re, Data => Data_Str, Matches => Real_Matches,
               Data_First => Real_Current);
            exit when Real_Matches (0) = GNAT.Regpat.No_Match;

            Find_Capture_Group;

            Real_Current := Real_Matches (0).Last + 1;
            Real_Index   := Real_Index + 1;
         end loop;
      end Process_Pattern_Groups;

      Named_Str     : constant String          := "\?<(\w+)>";
      Named_Re      : constant Pattern_Matcher := Compile (Named_Str);
      Named_Cg      : constant Match_Count     := Paren_Count (Named_Re);
      Named_Matches : Match_Array (0 .. Named_Cg);
   begin
      loop
         GNAT.Regpat.Match
           (Self    => Named_Re, Data => To_String (Proc_Str),
            Matches => Named_Matches);
         exit when Named_Matches (0) = GNAT.Regpat.No_Match;

         Ret.Names.Append
           (Capture_Group'
              (Name =>
                 Unbounded_Slice
                   (Source => Proc_Str, Low => Named_Matches (1).First,
                    High   => Named_Matches (1).Last),
               Index    => 0,  --  This is not the final index!
               Location =>
                 Match_Location'
                   (First => Named_Matches (0).First - 1,
         --  This is not the final location!
         Last => 0)));

         Delete
           (Source  => Proc_Str, From => Named_Matches (0).First,
            Through => Named_Matches (0).Last);

      end loop;

      Process_Pattern_Groups;

      Ret.Pattern := Pattern_Holder.To_Holder (Compile (To_String (Proc_Str)));

      return Ret;
   end Compile;

   -----------
   -- Match --
   -----------

   function Match (Self : Basic_Regex; Str : String) return Match_Obj is
      Cg : constant Match_Count :=
        Paren_Count (Regexp => Self.Pattern.Element);
      M : Match_Array (0 .. Cg);
   begin
      GNAT.Regpat.Match
        (Self => Self.Pattern.Element, Data => Str, Matches => M);

      return
        Match_Obj'
          (Matches         => Match_Holder.To_Holder (M), Names => Self.Names,
           Original_String => To_Unbounded_String (Str));
   end Match;

   --------------
   -- No_Match --
   --------------

   function No_Match (Self : Match_Obj) return Boolean is
   begin
      return Self.Matches.Element (0) = GNAT.Regpat.No_Match;
   end No_Match;

   ------------
   -- Length --
   ------------

   function Length (Self : Match_Obj) return Natural is
   begin
      return Self.Matches.Element'Length;
   end Length;

   ------------------
   -- Get_Noexcept --
   ------------------

   function Get_Noexcept (Self : Match_Obj; Index : String) return String is
   begin
      for I of Self.Names loop
         if I.Name = Index then
            return Get (Self => Self, Index => I.Index);
         end if;
      end loop;

      --  if we get here, there's no named group with name Str
      return No_Group_Name;
   end Get_Noexcept;

   ---------
   -- Get --
   ---------

   function Get (Self : Match_Obj; Index : String) return String is
      Ret : constant String := Get_Noexcept (Self => Self, Index => Index);
   begin
      if Ret = No_Group_Name then
         raise Unknown_Group_Name;
      end if;

      return Ret;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Self : Match_Obj; Index : Natural) return String is
      Loc : constant Match_Location := Self.Matches.Element (Index);
   begin
      return
        Slice
          (Source => Self.Original_String, Low => Loc.First, High => Loc.Last);
   end Get;

   ----------------------
   -- Get_Capture_Name --
   ----------------------

   function Get_Capture_Name (Self : Match_Obj; Index : Natural) return String
   is
   begin
      for I of Self.Names loop
         if I.Index = Index then
            return To_String (I.Name);
         end if;
      end loop;

      return "";
   end Get_Capture_Name;

end Wrapping.Regex;
