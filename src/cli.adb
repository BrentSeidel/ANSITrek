--
--  Author: Brent Seidel
--  Date: 15-Apr-2025
--
--  This file is part of ANSI Trek.
--  SimCPU is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  SimCPU is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with ANSI Trek. If not, see <https://www.gnu.org/licenses/>.
--
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with BBS.ANSI;
package body cli is
   --
   --  Put any needed initializations here
   --
   procedure init is
   begin
      null;
   end;
   --
   -- Main command loop
   --
   procedure cmds is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      prompt : constant String := BBS.ANSI.posCursor(25, 1) & "CMD>";
   begin
      loop
         Ada.Text_IO.Put(prompt);
         Ada.Text_IO.Unbounded_IO.Get_Line(rest);
         --
         --  Prepare command string
         --
         rest  := trim(rest);
         split(first, rest);
         Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
         if Ada.Strings.Unbounded.Length(first) = 0 then
            null;
         elsif first = "QUIT" then
            Ada.Text_IO.Put_Line(BBS.ANSI.rst);
            return;
         else
            Ada.Text_IO.Put_Line("Unrecognized command <" & Ada.Strings.Unbounded.To_String(first) & ">");
         end if;
      end loop;
   end;
   --
   --  Split on whitespace.  String is passed in in "rest".  The next
   --  token is returned in "first" and "rest" is updated to have that
   --  token removed.
   --
   procedure split(first : out Ada.Strings.Unbounded.Unbounded_String;
                   rest : in out Ada.Strings.Unbounded.Unbounded_String) is
      index : Natural;
   begin
      index := ada.Strings.Unbounded.Index(rest, whitespace);
      if index = 0 then
         first := rest;
         rest  := Ada.Strings.Unbounded.Null_Unbounded_String;
      else
         first := Ada.Strings.Unbounded.Unbounded_Slice(rest, 1, index - 1);
         rest  := Ada.Strings.Unbounded.Unbounded_Slice(rest, index + 1,
                                                        Ada.Strings.Unbounded.Length(rest));
         rest := trim(rest);
      end if;
   end;
   --
   --  Discard any leading spaces
   --
   function trim(s : Ada.Strings.Unbounded.Unbounded_String) return Ada.Strings.Unbounded.Unbounded_String is
      index : Natural;
   begin
      index := Ada.Strings.Unbounded.Index(s, blackspace);
      if index > 0 then
         return Ada.Strings.Unbounded.Unbounded_Slice(s, index, Ada.Strings.Unbounded.Length(s));
      else
         return s;
      end if;
   end;
end cli;
