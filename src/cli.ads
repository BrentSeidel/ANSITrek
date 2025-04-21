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
with Ada.Strings.Maps.Constants;
use type Ada.Strings.Maps.Character_Set;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with data;
use type data.sector_size;
use type data.universe_size;
use type data.sr_data;
use type data.location;
package cli is
   --
   --  Put any needed initializations here
   --
   procedure init;
   --
   -- Main command loop
   --
   procedure cmds;
   --
   --  Split string on whitespace
   --
   procedure split(first : out Ada.Strings.Unbounded.Unbounded_String;
                   rest : in out Ada.Strings.Unbounded.Unbounded_String);
   --
   --  Discard any leading spaces
   --
   function trim(s : Ada.Strings.Unbounded.Unbounded_String) return Ada.Strings.Unbounded.Unbounded_String;
   --
   --  Actions
   --
   procedure lr_scan;
   procedure move(r : Ada.Strings.Unbounded.Unbounded_String);
   procedure jump(r : Ada.Strings.Unbounded.Unbounded_String);
   procedure dock;
   procedure torpedo(r : Ada.Strings.Unbounded.Unbounded_String);
   --
   function get_galaxy_coords(r : Ada.Strings.Unbounded.Unbounded_String; v : out Boolean) return data.lr_pos;
   function get_sector_coords(r : Ada.Strings.Unbounded.Unbounded_String; v : out Boolean) return data.sr_pos;
   --
   procedure update_msg;
private
   --
   --  Space, back-space, tab, line-feed, vertical-tab, form-feed, and carriage return
   --
   whitespace : constant Ada.Strings.Maps.Character_Set :=
         Ada.Strings.Maps.To_Set(' ' & Character'Val(8) & Character'Val(9) &
         Character'Val(10) & Character'Val(11) & Character'Val(12) & Character'Val(13));
   --
   blackspace : constant Ada.Strings.Maps.Character_Set := not whitespace;
   --
   --  Check if ship is adjacent to object of type o.
   --
   function check_adjacent(o : data.sr_data) return Boolean;
end cli;
