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
with Ada.Strings.Unbounded;
with data;
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
   ship_pos  : location := (row => 13, col => 1);
   ship_size : size := (row => 10, col => 15);
   --
   sect_pos  : location := (row => 1, col => 1);
   sect_size : size := (row => 11, col => 21);
   --
   univ_pos  : location := (row => 1, col => 23);
   univ_size : size := (row => 11, col => 51);
   --
   cas_pos  : location := (row => 13, col => 17);
   cas_size : size := (row => 10, col => 40);
   --
   planet_pos  : location := (row => 14, col => 58);
   planet_size : size := (row => 12, col => 20);
   --
   enemy_pos  : location := (row => 1, col => 75);
   enemy_size : size := (row => 12, col => 20);
   --
   --  For messages
   --
   msg : Ada.Strings.Unbounded.Unbounded_String;
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
   procedure draw_ship;
   --
   --  Draw the sector map
   --
   procedure draw_sect;
   --
   --  Draw the galaxy map
   --
   procedure draw_univ;
   --
   --  Draw the CAS window
   --
   procedure draw_cas;
   --
   --  Draw the planets window
   --
   procedure draw_planet;
   --
   --  Draw the enemies window
   --
   procedure draw_enemy;
private
   --
   --  Utility function to write sector position on screen
   --
   procedure sr_put_pos(p : data.sr_pos);
   --
   --  Draw a window frame and title
   --
   procedure frame(p : location; s : size; t : String);
end screen;
