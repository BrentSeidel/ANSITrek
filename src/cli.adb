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
with cas;
with screen;
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
         --
         --  Special stuff to do while docked
         --
         if data.ship.loc = data.docked then
            data.ship.energy  := data.full_fuel;
            data.ship.torpedo := data.full_torp;
         end if;
         --
         --  Special stuff to do while in orbit
         --
         null;
         --
         --  Other updates
         --
         update_msg;
         screen.redraw;
         cas.clear_once;
         --
         --  Get and process commands
         --
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
         elsif first = "LR" then
            lr_scan;
         elsif first = "JUMP" then
            jump(rest);
         elsif first = "MOVE" then
            move(rest);
         elsif first = "DOCK" then
            dock;
         elsif first = "TORP" then
            torpedo(rest);
         elsif first = "QUIT" then
            Ada.Text_IO.Put_Line(BBS.ANSI.rst);
            return;
         else
            cas.set_msg(cas.cmd_unknown, cas.info, True);
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
   --
   --  Actions
   --
   procedure lr_scan is
      x : constant data.universe_size := data.ship.pos_lr.x;
      y : constant data.universe_size := data.ship.pos_lr.y;
   begin
      if x > data.universe_size'First then
         if y > data.universe_size'First then
            data.u(x - 1, y - 1).discover := True;
         end if;
         data.u(x - 1, y).discover := True;
         if y < data.universe_size'Last then
            data.u(x - 1, y + 1).discover := True;
         end if;
      end if;
      if y > data.universe_size'First then
         data.u(x, y - 1).discover := True;
      end if;
      data.u(x, y).discover := True;
      if y < data.universe_size'Last then
         data.u(x, y + 1).discover := True;
      end if;
      if x < data.universe_size'Last then
         if y > data.universe_size'First then
            data.u(x + 1, y - 1).discover := True;
         end if;
         data.u(x + 1, y).discover := True;
         if y < data.universe_size'Last then
            data.u(x + 1, y + 1).discover := True;
         end if;
      end if;
   end;
   --
   procedure move(r : Ada.Strings.Unbounded.Unbounded_String) is
      valid : Boolean;
      pos : data.sr_pos := get_sector_coords(r, valid);
   begin
      if valid then
         if (data.sect(pos.x, pos.y) = data.empty) then
            data.ship.energy  := data.ship.energy - data.move_energy;
            data.ship.elapsed := data.ship.elapsed + data.move_time;
            data.sect(data.ship.pos_sr.x, data.ship.pos_sr.y) := data.empty;
            data.ship.pos_sr := pos;
            data.sect(pos.x, pos.y) := data.self;
            data.ship.loc := data.space;
         else
            cas.set_msg(cas.occupied, cas.info, True);
         end if;
      end if;
   end;
   --
   procedure dock is
      base_detected : Boolean := False;
      pos : data.sr_pos := data.ship.pos_sr;
   begin
      --
      --  Check for adjacent starbase
      --
      if pos.x > data.sector_size'First then
         if pos.y > data.sector_size'First then
            if data.sect(pos.x - 1, pos.y - 1) = data.base then
               base_detected := True;
            end if;
         end if;
         if data.sect(pos.x - 1, pos.y) = data.base then
            base_detected := True;
         end if;
         if pos.y < data.sector_size'Last then
            if data.sect(pos.x - 1, pos.y + 1) = data.base then
               base_detected := True;
            end if;
         end if;
      end if;
      if pos.y > data.sector_size'First then
         if data.sect(pos.x, pos.y - 1) = data.base then
            base_detected := True;
         end if;
      end if;
      if pos.y < data.sector_size'Last then
         if data.sect(pos.x, pos.y + 1) = data.base then
            base_detected := True;
         end if;
      end if;
      if pos.x < data.sector_size'Last then
         if pos.y > data.sector_size'First then
            if data.sect(pos.x + 1, pos.y - 1) = data.base then
               base_detected := True;
            end if;
         end if;
         if data.sect(pos.x + 1, pos.y) = data.base then
            base_detected := True;
         end if;
         if pos.y < data.sector_size'Last then
            if data.sect(pos.x + 1, pos.y + 1) = data.base then
               base_detected := True;
            end if;
         end if;
      end if;
      --
      --  If detected then do docking process
      --
      if base_detected then
         data.ship.loc := data.docked;
      else
         cas.set_msg(cas.no_dock, cas.warning, True);
      end if;
   end;
   --
   --  Launch a torpedo at a target
   --
   procedure torpedo(r : Ada.Strings.Unbounded.Unbounded_String) is
      valid : Boolean;
      pos : data.sr_pos := get_sector_coords(r, valid);
      lr : data.lr_pos := data.ship.pos_lr;
      target : data.sr_data;
   begin
      if data.ship.torpedo = 0 then
         cas.set_msg(cas.no_torp, cas.warning, True);
         return;
      end if;
      if valid then
         target := data.sect(pos.x, pos.y);
         data.ship.torpedo := data.ship.torpedo - 1;
         data.sect(pos.x, pos.y) := data.empty;
         if target = data.self then
            data.ship.energy := 0;
            cas.set_msg(cas.dest_self, cas.alert, False);
         elsif target = data.base then
            cas.set_msg(cas.dest_base, cas.alert, True);
            data.u(lr.x, lr.y).base := False;
            if data.ship.loc = data.docked then
               data.ship.loc := data.space;
            end if;
         elsif target = data.planet then
            cas.set_msg(cas.dest_planet, cas.alert, True);
            data.u(lr.x, lr.y).planets := data.u(lr.x, lr.y).planets - 1;
         elsif target = data.star then
            cas.set_msg(cas.dest_star, cas.alert, True);
            data.u(lr.x, lr.y).stars := data.u(lr.x, lr.y).stars - 1;
         elsif target = data.enemy1 then
            cas.set_msg(cas.dest_enemy1, cas.alert, True);
            data.u(lr.x, lr.y).enemies := data.u(lr.x, lr.y).enemies - 1;
         elsif target = data.empty then
            cas.set_msg(cas.dest_empty, cas.info, True);
         end if;
      end if;
   end;
   --
   procedure jump(r : Ada.Strings.Unbounded.Unbounded_String) is
      valid : Boolean;
      pos : data.lr_pos := get_galaxy_coords(r, valid);
   begin
      if valid then
         data.ship.energy  := data.ship.energy - data.jump_energy;
         data.ship.elapsed := data.ship.elapsed + data.jump_time;
         data.ship.pos_lr := pos;
         data.ship.pos_sr.x := data.sector_size'Last/2;
         data.ship.pos_sr.y := data.sector_size'Last/2;
         data.init_sr(pos.x, pos.y);
         data.ship.loc := data.space;
      end if;
   end;
   --
   function get_galaxy_coords(r : Ada.Strings.Unbounded.Unbounded_String; v : out Boolean) return data.lr_pos is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String := r;
      pos   : data.lr_pos := (data.universe_size'First, data.universe_size'First);
      temp  : Integer;
   begin
      v := True;
      split(first, rest);
      temp := Integer'Value(Ada.Strings.Unbounded.To_String(first));
      if (temp >= Integer(data.universe_size'First)) and (temp <= Integer(data.universe_size'Last)) then
         pos.x := data.universe_size(temp);
      else
         v := False;
         return pos;
      end if;
      split(first, rest);
      temp := Integer'Value(Ada.Strings.Unbounded.To_String(first));
      if (temp >= Integer(data.universe_size'First)) and (temp <= Integer(data.universe_size'Last)) then
         pos.y := data.universe_size(temp);
      else
         v := False;
         return pos;
      end if;
      return pos;
   end;
   --
   function get_sector_coords(r : Ada.Strings.Unbounded.Unbounded_String; v : out Boolean) return data.sr_pos is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String := r;
      pos   : data.sr_pos := (data.sector_size'First, data.sector_size'First);
      temp  : Integer;
   begin
      v := True;
      split(first, rest);
      temp := Integer'Value(Ada.Strings.Unbounded.To_String(first));
      if (temp >= Integer(data.sector_size'First)) and (temp <= Integer(data.sector_size'Last)) then
         pos.x := data.sector_size(temp);
      else
         v := False;
         return pos;
      end if;
      split(first, rest);
      temp := Integer'Value(Ada.Strings.Unbounded.To_String(first));
      if (temp >= Integer(data.sector_size'First)) and (temp <= Integer(data.sector_size'Last)) then
         pos.y := data.sector_size(temp);
      else
         v := False;
         return pos;
      end if;
      return pos;
   end;
   --
   --  Update CAS messages
   --
   procedure update_msg is
   begin
      if data.u(data.ship.pos_lr.x, data.ship.pos_lr.y).enemies > 0 then
         cas.set_msg(cas.enemies, cas.alert, False);
      else
         cas.clear_msg(cas.enemies);
      end if;
      if data.ship.loc = data.docked then
         cas.clear_msg(cas.in_orbit);
         cas.set_msg(cas.docked, cas.info, False);
      elsif data.ship.loc = data.orbit then
         cas.clear_msg(cas.docked);
         cas.set_msg(cas.in_orbit, cas.info, False);
      else
         cas.clear_msg(cas.in_orbit);
         cas.clear_msg(cas.docked);
      end if;
   end;
   --
end cli;
