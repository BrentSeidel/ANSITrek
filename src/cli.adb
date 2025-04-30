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
      prompt : constant String := BBS.ANSI.posCursor(27, 1) & "CMD>";
   begin
      loop
         --
         --  Check for exit condition either your ship is desroyed or all enemies
         --  have been destroyed.
         --
         if data.ship.energy = 0 then
            Ada.Text_IO.Put_Line("Your ship has been destroyed.  The game is over." & BBS.ANSI.rst);
            exit;
         end if;
         if data.total_enemies = 0 then
            Ada.Text_IO.Put_Line("All enemies have been destroyed.  You have won." & BBS.ANSI.rst);
            exit;
         end if;
         --
         --  Special stuff to do while docked
         --
         if data.ship.loc = data.docked then
            if data.ship.energy < data.full_fuel then
               data.ship.energy  := data.full_fuel;
            end if;
            if data.ship.torpedo < data.full_torp then
               data.ship.torpedo := data.full_torp;
            end if;
            if data.ship.crew < data.full_crew then
               data.ship.crew := data.full_crew;
            end if;
         end if;
         --
         --  Special stuff to do while in orbit
         --
         null;
--         Ada.Text_IO.Unbounded_IO.Get_Line(rest);  --  This is here for debugging.
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
         elsif first = "ORBIT" then
            orbit;
         elsif first = "TORP" then
            torpedo(rest);
         elsif first = "SHIELDS" then
            shields(rest);
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
      x : constant data.galaxy_size := data.ship.pos_lr.x;
      y : constant data.galaxy_size := data.ship.pos_lr.y;
   begin
      if x > data.galaxy_size'First then
         if y > data.galaxy_size'First then
            data.u(x - 1, y - 1).discover := True;
         end if;
         data.u(x - 1, y).discover := True;
         if y < data.galaxy_size'Last then
            data.u(x - 1, y + 1).discover := True;
         end if;
      end if;
      if y > data.galaxy_size'First then
         data.u(x, y - 1).discover := True;
      end if;
      data.u(x, y).discover := True;
      if y < data.galaxy_size'Last then
         data.u(x, y + 1).discover := True;
      end if;
      if x < data.galaxy_size'Last then
         if y > data.galaxy_size'First then
            data.u(x + 1, y - 1).discover := True;
         end if;
         data.u(x + 1, y).discover := True;
         if y < data.galaxy_size'Last then
            data.u(x + 1, y + 1).discover := True;
         end if;
      end if;
   end;
   --
   --  Move within a sector
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
      data.attack_ship;
   end;
   --
   --  Move to another sector
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
   --  Dock at a starbase
   --
   procedure dock is
      pos : data.sr_pos := data.ship.pos_sr;
   begin
      --
      --  If adjacent to star base then do docking process
      --
      if check_adjacent(data.base) then
         data.ship.loc := data.docked;
      else
         cas.set_msg(cas.no_dock, cas.warning, True);
      end if;
   end;
   --
   --  Orbit a planet
   --
   procedure orbit is
      pos : data.sr_pos := data.ship.pos_sr;
   begin
      --
      --  If adjacent to planet then orbit
      --
      if check_adjacent(data.planet) then
         data.ship.loc := data.orbit;
      else
         cas.set_msg(cas.no_orbit, cas.warning, True);
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
         if target = data.self then
            data.ship.energy := 0;
            cas.set_msg(cas.dest_self, cas.alert, False);
            data.sect(pos.x, pos.y) := data.empty;
         elsif target = data.base then
            cas.set_msg(cas.dest_base, cas.alert, True);
            data.u(lr.x, lr.y).base := False;
            if data.ship.loc = data.docked then
               data.ship.loc := data.space;
            end if;
            data.sect(pos.x, pos.y) := data.empty;
         elsif target = data.planet then
            data.attack_planet(pos, data.torp_energy);
         elsif target = data.star then
            cas.set_msg(cas.dest_star, cas.alert, True);
            data.u(lr.x, lr.y).stars := data.u(lr.x, lr.y).stars - 1;
            data.sect(pos.x, pos.y) := data.empty;
         elsif target = data.enemy1 then
            data.attack_enemy(pos, data.torp_energy);
         elsif target = data.empty then
            cas.set_msg(cas.dest_empty, cas.info, True);
         end if;
      end if;
      data.attack_ship;
   end;
   --
   --  Control shields
   --
   procedure shields(r : Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String := r;
      temp  : Integer;
   begin
      split(first, rest);
      Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
      if first = "UP" then
         data.ship.shields := True;
      elsif first = "DOWN" then
         data.ship.shields := False;
      else
         temp := Integer'Value(Ada.Strings.Unbounded.To_String(first));
         if (temp >= 0) and (temp < data.ship.energy) then
            data.ship.shield := Natural(temp);
         else
            cas.set_msg(cas.invalid, cas.info, True);
         end if;
      end if;
   end;
   --
   function get_galaxy_coords(r : Ada.Strings.Unbounded.Unbounded_String; v : out Boolean) return data.lr_pos is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String := r;
      pos   : data.lr_pos := (data.galaxy_size'First, data.galaxy_size'First);
      temp  : Integer;
   begin
      v := True;
      split(first, rest);
      temp := Integer'Value(Ada.Strings.Unbounded.To_String(first));
      if (temp >= Integer(data.galaxy_size'First)) and (temp <= Integer(data.galaxy_size'Last)) then
         pos.x := data.galaxy_size(temp);
      else
         cas.set_msg(cas.invalid, cas.info, True);
         v := False;
         return pos;
      end if;
      split(first, rest);
      temp := Integer'Value(Ada.Strings.Unbounded.To_String(first));
      if (temp >= Integer(data.galaxy_size'First)) and (temp <= Integer(data.galaxy_size'Last)) then
         pos.y := data.galaxy_size(temp);
      else
         cas.set_msg(cas.invalid, cas.info, True);
         v := False;
         return pos;
      end if;
      return pos;
   exception
      when others =>
         cas.set_msg(cas.invalid, cas.info, True);
         v := False;
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
         cas.set_msg(cas.invalid, cas.info, True);
         v := False;
         return pos;
      end if;
      split(first, rest);
      temp := Integer'Value(Ada.Strings.Unbounded.To_String(first));
      if (temp >= Integer(data.sector_size'First)) and (temp <= Integer(data.sector_size'Last)) then
         pos.y := data.sector_size(temp);
      else
         cas.set_msg(cas.invalid, cas.info, True);
         v := False;
         return pos;
      end if;
      return pos;
   exception
      when others =>
         cas.set_msg(cas.invalid, cas.info, True);
         v := False;
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
   --  Check if ship is adjacent to object of type o.
   --
   function check_adjacent(o : data.sr_data) return Boolean is
      pos : data.sr_pos := data.ship.pos_sr;
   begin
      if pos.x > data.sector_size'First then
         if pos.y > data.sector_size'First then
            if data.sect(pos.x - 1, pos.y - 1) = o then
               return True;
            end if;
         end if;
         if data.sect(pos.x - 1, pos.y) = o then
           return True;
         end if;
         if pos.y < data.sector_size'Last then
            if data.sect(pos.x - 1, pos.y + 1) = o then
               return True;
            end if;
         end if;
      end if;
      if pos.y > data.sector_size'First then
         if data.sect(pos.x, pos.y - 1) = o then
            return True;
         end if;
      end if;
      if pos.y < data.sector_size'Last then
         if data.sect(pos.x, pos.y + 1) = o then
            return True;
         end if;
      end if;
      if pos.x < data.sector_size'Last then
         if pos.y > data.sector_size'First then
            if data.sect(pos.x + 1, pos.y - 1) = o then
               return True;
            end if;
         end if;
         if data.sect(pos.x + 1, pos.y) = o then
            return True;
         end if;
         if pos.y < data.sector_size'Last then
            if data.sect(pos.x + 1, pos.y + 1) = o then
               return True;
            end if;
         end if;
      end if;
      return False;
   end;
end cli;
