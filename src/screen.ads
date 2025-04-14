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
package screen is
   --
   type location is record
      row : Natural;
      col : Natural;
   end record;
   --
   type size is record
      row : Natural;
      col : Natural;
   end record;
   --
   --  Values for locations and sizes
   --
   screen_size : size;
   ship_pos  : location := (row => 1, col => 1);
   ship_size : size := (row => 6, col => 15);
   --
   --  Initialization only needs to be called once.
   --
   procedure init;
   --
   --  Redraws the entire screen
   --
   procedure redraw;
   --
   --  Draw ship state
   --
   procedure redraw_ship;
end screen;
