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
package body screen is
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
      redraw_ship;
      Ada.Text_IO.Put(BBS.ANSI.posCursor(45, 1));
   end;
   --
   --  Draw ship state
   --
   procedure redraw_ship is
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
   end;
end screen;
