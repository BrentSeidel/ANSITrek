--
--  Author: Brent Seidel
--  Date: 14-Apr-2025
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
with BBS.ANSI;
use type BBS.ANSI.term_type;
with cas;
use type cas.msg_prio;
with data;
use type data.sector_size;
use type data.galaxy_size;
package body screen is
   package nat_io is new Ada.Text_IO.Integer_IO(Natural);
   package sect_io is new Ada.Text_IO.Integer_IO(data.sector_size);
   package univ_io is new Ada.Text_IO.Integer_IO(data.galaxy_size);
   --
   --  Initialization only needs to be called once.
   --
   procedure init is
      term : BBS.ANSI.term_type := BBS.ANSI.identify;
      row : Natural;
      col : Natural;
   begin
      if term = BBS.ANSI.unknown then
         Ada.Text_IO.Put_Line("Cannot recognize terminal type.");
         return;
      end if;
      BBS.ANSI.getSize(row, col);
      screen_size.row := row;
      screen_size.col := col;
      Ada.Text_IO.Put(BBS.ANSI.g1_sym);
      Ada.Text_IO.Put(BBS.ANSI.white);
   end;
   --
   --  Redraws the entire screen
   --
   procedure redraw is
   begin
      Ada.Text_IO.Put(BBS.ANSI.cls);
      draw_sect(wSect);
      draw_univ(wUniv);
      draw_ship(wShip);
      draw_cas(wCas);
      draw_planet(wPlanet);
      draw_enemy(wEnemy);
   end;
   --
   --  Draw ship state
   --
   procedure draw_ship(w : window) is
   begin
      frame(w);
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 1, w.lCol + 1) & "Status");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 1, w.lCol + 8));
      case data.ship.status is
         when data.blue =>
            Ada.Text_IO.Put(BBS.ANSI.blue    & "   BLUE");
         when data.green =>
            Ada.Text_IO.Put(BBS.ANSI.green   & "  GREEN");
         when data.red =>
            Ada.Text_IO.Put(BBS.ANSI.red     & "    RED");
         when data.yellow =>
            Ada.Text_IO.Put(BBS.ANSI.yellow  & " YELLOW");
         when data.cyan =>
            Ada.Text_IO.Put(BBS.ANSI.cyan    & "   CYAN");
         when data.magenta =>
            Ada.Text_IO.Put(BBS.ANSI.magenta & "MAGENTA");
      end case;
      Ada.Text_IO.Put(BBS.ANSI.white);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 2, w.lCol + 1) & "Sector");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 2, w.lCol + 10));
      univ_io.Put(data.ship.pos_lr.x, width => 2, base => 10);
      Ada.Text_IO.Put(",");
      univ_io.Put(data.ship.pos_lr.y, width => 2, base => 10);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 3, w.lCol + 1) & "Position");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 3, w.lCol + 10));
      sr_put_pos(data.ship.pos_sr);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 4, w.lCol + 1) & "Energy");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 4, w.lCol + 9));
      nat_io.Put(data.ship.energy, width => 6, base => 10);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 5, w.lCol + 1) & "Shields");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 5, w.lCol + 8));
      nat_io.Put(data.ship.shield, width => 6, base => 10);
      if data.ship.shields then
         Ada.Text_IO.Put(BBS.ANSI.green & "U");
      else
         Ada.Text_IO.Put(BBS.ANSI.red & "D");
      end if;
      Ada.Text_IO.Put(BBS.ANSI.white);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 6, w.lCol + 1) & "Torpedos");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 6, w.lCol + 9));
      nat_io.Put(data.ship.torpedo, width => 6, base => 10);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 7, w.lCol + 1) & "Time");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 7, w.lCol + 9));
      nat_io.Put(data.ship.elapsed, width => 6, base => 10);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 8, w.lCol + 1) & "Crew");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + 8, w.lCol + 9));
      nat_io.Put(data.ship.crew, width => 6, base => 10);
   end;
   --
   --  Draw the sector map
   --
   procedure draw_sect(w : window) is
   begin
      frame(w);
      for i in data.sector_size'Range loop
         Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + Natural(i), w.lCol + 1));
         for j in data.sector_size'Range loop
            case data.sect(j, i) is
               when data.empty =>
                  Ada.Text_IO.Put(" .");
               when data.star =>
                  Ada.Text_IO.Put(BBS.ANSI.yellow & " *" & BBS.ANSI.white);
               when data.base =>
                  Ada.Text_IO.Put(BBS.ANSI.green & " B" & BBS.ANSI.white);
               when data.enemy1 =>
                  Ada.Text_IO.Put(BBS.ANSI.red & " 1" & BBS.ANSI.white);
               when data.enemy2 =>
                  Ada.Text_IO.Put(BBS.ANSI.red & " 2" & BBS.ANSI.white);
               when data.enemy3 =>
                  Ada.Text_IO.Put(BBS.ANSI.red & " 3" & BBS.ANSI.white);
               when data.planet =>
                  Ada.Text_IO.Put(BBS.ANSI.blue & " O" & BBS.ANSI.white);
               when data.self =>
                  Ada.Text_IO.Put(BBS.ANSI.cyan & " +" & BBS.ANSI.white);
            end case;
         end loop;
      end loop;
   end;
   --
   --  Draw the galaxy map
   --
   procedure draw_univ(w : window) is
      sect : data.lr_data;
   begin
      frame(w);
      for i in data.galaxy_size'Range loop
         Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + Natural(i), w.lCol + 1));
         for j in data.galaxy_size'Range loop
            if (j = data.ship.pos_lr.x) and (i = data.ship.pos_lr.y) then
               Ada.Text_IO.Put("[");
            else
               Ada.Text_IO.Put(" ");
            end if;
            --
            sect := data.u(j, i);
            if not sect.discover then
               Ada.Text_IO.Put("---");
            elsif sect.destroyed then
               Ada.Text_IO.Put("***");
            else
               if sect.enemies > 0 then
                  Ada.Text_IO.Put(BBS.ANSI.red);
               elsif sect.base then
                  Ada.Text_IO.Put(BBS.ANSI.green);
               end if;
               nat_io.put(sect.enemies, width => 1, base => 10);
               if sect.base then
                  Ada.Text_IO.Put("1");
               else
                  Ada.Text_IO.Put("0");
               end if;
               nat_io.Put(sect.stars, width => 1, base => 10);
               Ada.Text_IO.Put(BBS.ANSI.white);
            end if;
            --
            if (j = data.ship.pos_lr.x) and (i = data.ship.pos_lr.y) then
               Ada.Text_IO.Put("]");
            else
               Ada.Text_IO.Put(" ");
            end if;
         end loop;
      end loop;
   end;
   --
   --  Draw the CAS window
   --
   procedure draw_cas(w : window) is
      line : Natural := 1;
   begin
      frame(w);
      --
      --  Alerts first
      --
      Ada.Text_IO.Put(BBS.ANSI.red);
      for m in cas.messages loop
         exit when line > w.sRow;
         if cas.msg_list(m).active and (cas.msg_list(m).priority = cas.alert) then
            Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + line, w.lCol + 1));
            Ada.Text_IO.Put(cas.msg_text(m));
            line := line + 1;
         end if;
      end loop;
      --
      --  Then warnings
      --
      Ada.Text_IO.Put(BBS.ANSI.yellow);
      for m in cas.messages loop
         exit when line > w.sRow;
         if cas.msg_list(m).active and (cas.msg_list(m).priority = cas.warning) then
            Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + line, w.lCol + 1));
            Ada.Text_IO.Put(cas.msg_text(m));
            line := line + 1;
         end if;
      end loop;
      --
      --  Finally info messages
      --
      Ada.Text_IO.Put(BBS.ANSI.white);
      for m in cas.messages loop
         exit when line > w.sRow;
         if cas.msg_list(m).active and (cas.msg_list(m).priority = cas.info) then
            Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + line, w.lCol + 1));
            Ada.Text_IO.Put(cas.msg_text(m));
            line := line + 1;
         end if;
      end loop;
   end;
   --
   --  Draw the planets window
   --
   procedure draw_planet(w : window) is
   begin
      frame(w);
      for i in 1 .. data.planet_count loop
         Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + i, w.lCol + 1));
         if data.planets(i).destr then
            Ada.Text_IO.Put(BBS.ANSI.red & "Destroyed" & BBS.ANSI.white);
         else
            if data.ship.orbit = i then
               Ada.Text_IO.Put(BBS.ANSI.green);
            end if;
            sr_put_pos(data.planets(i).pos);
            if data.ship.orbit = i then
               Ada.Text_IO.Put(BBS.ANSI.white);
            end if;
            Ada.Text_IO.Put("  ");
            if data.planets(i).scanned then
               nat_io.Put(data.planets(i).fuel, width => 4, base => 10);
            else
               Ada.Text_IO.Put(" ---");
            end if;
         end if;
      end loop;
   end;
   --
   --  Draw the enemies window
   --
   procedure draw_enemy(w : window) is
   begin
      frame(w);
      for i in 1 .. data.enemy_count loop
         Ada.Text_IO.Put(BBS.ANSI.posCursor(w.lRow + i, w.lCol + 1));
         if data.enemies(i).destr then
            Ada.Text_IO.Put_Line(BBS.ANSI.red & "Destroyed" & BBS.ANSI.white);
         else
            sr_put_pos(data.enemies(i).pos);
            Ada.Text_IO.Put("  ");
            nat_io.Put(data.enemies(i).energy, width => 4, base => 10);
            Ada.Text_IO.Put("  ");
            if data.enemies(i).shot > 0 then
               Ada.Text_IO.Put(BBS.ANSI.red);
               nat_io.Put(data.enemies(i).shot, width => 4, base => 10);
               Ada.Text_IO.Put(BBS.ANSI.white);
            else
               Ada.Text_IO.Put("----");
            end if;
         end if;
      end loop;
   end;
   --
   --  Draw a message window
   --
   --  For now, this supports a single line message less than the screen width.
   --  The intent is to eventually implement text wrapping and allow multiline
   --  messages.
   --
   procedure draw_msg(m : String) is
      temp   : Ada.Strings.Unbounded.Unbounded_String;
      lMsg   : Natural := m'Length;
      lTitle : Natural;
   begin
      wMsg.title := Ada.Strings.Unbounded.To_Unbounded_String("Message");
      lTitle := Ada.Strings.Unbounded.Length(wMsg.title);
      wMsg.sCol := lMsg + 1;
      wMsg.sRow := 2;
      wMsg.lCol := (screen_size.col - wMsg.sCol)/2;
      wMsg.lRow := (screen_size.row - wMsg.sRow)/2;
      frame(wMsg);
      Ada.Text_IO.Put(BBS.ANSI.posCursor(wMsg.lRow + 1, wMsg.lCol + 1));
      Ada.Text_IO.Put(m);
      Ada.Text_IO.Unbounded_IO.Get_Line(temp);
   end;
   --
   --  Utility function to write sector position on screen
   --
   procedure sr_put_pos(p : data.sr_pos) is
   begin
      sect_io.Put(p.x, width => 2, base => 10);
      Ada.Text_IO.Put(",");
      sect_io.Put(p.y, width => 2, base => 10);
   end;
   --
   --  Draw a window frame and title
   --
   procedure frame(w : window) is
      len : Natural := Ada.Strings.Unbounded.Length(w.title);
      r   : Natural := w.lRow;
      c   : Natural := (w.sCol - len)/2 + w.lCol;
   begin
      Ada.Text_IO.Put(BBS.ANSI.so);
      Ada.Text_IO.Put(BBS.ANSI.drawBox(w.lRow, w.lCol, w.sRow, w.sCol, True));
      Ada.Text_IO.Put(BBS.ANSI.si);
      Ada.Text_IO.Put(BBS.ANSI.posCursor(r, c) & Ada.Strings.Unbounded.To_String(w.title));
   end;
   --
end screen;
