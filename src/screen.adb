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
with data;
use type data.sector_size;
use type data.universe_size;
package body screen is
   package nat_io is new Ada.Text_IO.Integer_IO(Natural);
   package sect_io is new Ada.Text_IO.Integer_IO(data.sector_size);
   package univ_io is new Ada.Text_IO.Integer_IO(data.universe_size);
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
      draw_ship;
      draw_sect;
      draw_univ;
--      Ada.Text_IO.Put(BBS.ANSI.posCursor(45, 1));
   end;
   --
   --  Draw ship state
   --
   procedure draw_ship is
   begin
      Ada.Text_IO.Put(BBS.ANSI.so);
      Ada.Text_IO.Put(BBS.ANSI.drawBox(ship_pos.row, ship_pos.col, ship_size.row, ship_size.col, True));
      Ada.Text_IO.Put(BBS.ANSI.si);
      Ada.Text_IO.Put(BBS.ANSI.posCursor(ship_pos.row + 1, ship_pos.col + 1) & "Status");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(ship_pos.row + 2, ship_pos.col + 1) & "Sector");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(ship_pos.row + 3, ship_pos.col + 1) & "Position");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(ship_pos.row + 4, ship_pos.col + 1) & "Energy");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(ship_pos.row + 5, ship_pos.col + 1) & "Shields");
      Ada.Text_IO.Put(BBS.ANSI.posCursor(ship_pos.row + 1, ship_pos.col + 8));
      --
      case data.ship.status is
         when data.blue =>
            Ada.Text_IO.Put(BBS.ANSI.blue & "BLUE");
         when data.green =>
            Ada.Text_IO.Put(BBS.ANSI.green & "GREEN");
         when data.red =>
            Ada.Text_IO.Put(BBS.ANSI.red & "RED");
         when data.yellow =>
            Ada.Text_IO.Put(BBS.ANSI.yellow & "YELLOW");
         when data.cyan =>
            Ada.Text_IO.Put(BBS.ANSI.cyan & "CYAN");
         when data.magenta =>
            Ada.Text_IO.Put(BBS.ANSI.magenta & "MAGENTA");
      end case;
      Ada.Text_IO.Put(BBS.ANSI.white);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(ship_pos.row + 2, ship_pos.col + 10));
      univ_io.Put(data.ship.pos_lr.x, width => 2, base => 10);
      Ada.Text_IO.Put(",");
      univ_io.Put(data.ship.pos_lr.y, width => 2, base => 10);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(ship_pos.row + 3, ship_pos.col + 10));
      sect_io.Put(data.ship.pos_sr.x, width => 2, base => 10);
      Ada.Text_IO.Put(",");
      sect_io.Put(data.ship.pos_sr.y, width => 2, base => 10);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(ship_pos.row + 4, ship_pos.col + 9));
      nat_io.Put(data.ship.energy, width => 6, base => 10);
      --
      Ada.Text_IO.Put(BBS.ANSI.posCursor(ship_pos.row + 5, ship_pos.col + 9));
      nat_io.Put(data.ship.shield, width => 6, base => 10);
   end;
   --
   --  Draw the sector map
   --
   procedure draw_sect is
   begin
      Ada.Text_IO.Put(BBS.ANSI.so);
      Ada.Text_IO.Put(BBS.ANSI.drawBox(sect_pos.row, sect_pos.col, sect_size.row, sect_size.col, True));
      Ada.Text_IO.Put(BBS.ANSI.si);
      for i in data.sector_size'Range loop
         Ada.Text_IO.Put(BBS.ANSI.posCursor(sect_pos.row + Natural(i), sect_pos.col + 1));
         for j in data.sector_size'Range loop
            case data.sect(i, j) is
               when data.empty =>
                  Ada.Text_IO.Put(" .");
               when data.star =>
                  Ada.Text_IO.Put(" *");
               when data.base =>
                  Ada.Text_IO.Put(" B");
               when data.enemy1 =>
                  Ada.Text_IO.Put(" 1");
               when data.enemy2 =>
                  Ada.Text_IO.Put(" 2");
               when data.enemy3 =>
                  Ada.Text_IO.Put(" 3");
               when data.planet =>
                  Ada.Text_IO.Put(" O");
               when data.self =>
                  Ada.Text_IO.Put(" +");
            end case;
         end loop;
      end loop;
   end;
   --
   --  Draw the galaxy map
   --
   procedure draw_univ is
      sect : data.lr_data;
   begin
      Ada.Text_IO.Put(BBS.ANSI.so);
      Ada.Text_IO.Put(BBS.ANSI.drawBox(univ_pos.row, univ_pos.col, univ_size.row, univ_size.col, True));
      Ada.Text_IO.Put(BBS.ANSI.si);
      for i in data.universe_size'Range loop
         Ada.Text_IO.Put(BBS.ANSI.posCursor(univ_pos.row + Natural(i), univ_pos.col + 1));
         for j in data.universe_size'Range loop
            if (j = data.ship.pos_lr.x) and (i = data.ship.pos_lr.y) then
               Ada.Text_IO.Put("[");
            else
               Ada.Text_IO.Put(" ");
            end if;
            --
            sect := data.u(i, j);
            if not sect.discover then
               Ada.Text_IO.Put("---");
            elsif sect.destroyed then
               Ada.Text_IO.Put("***");
            else
               nat_io.put(sect.enemies, width => 1, base => 10);
               if sect.base then
                  Ada.Text_IO.Put("1");
               else
                  Ada.Text_IO.Put("0");
               end if;
               nat_io.Put(sect.stars, width => 1, base => 10);
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
end screen;
