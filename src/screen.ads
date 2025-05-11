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
   type window is record
      lRow : Natural;  --  Location row
      lCol : Natural;  --  Location column
      sRow : Natural;  --  Size in rows
      sCol : Natural;  --  Size in columns
   end record;
   --
   --  Values for locations and sizes
   --
   screen_size : size;
   --
   wShip   : window := (lRow => 13, lCol =>  1, sRow => 10, sCol => 15);
   wSect   : window := (lRow =>  1, lCol =>  1, sRow => 11, sCol => 21);
   wUniv   : window := (lRow =>  1, lCol => 23, sRow => 11, sCol => 51);
   wCas    : window := (lRow => 13, lCol => 17, sRow => 10, sCol => 40);
   wPlanet : window := (lRow => 14, lCol => 58, sRow => 12, sCol => 20);
   wEnemy  : window := (lRow =>  1, lCol => 75, sRow => 12, sCol => 20);
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
   procedure draw_ship(w : window);
   --
   --  Draw the sector map
   --
   procedure draw_sect(w : window);
   --
   --  Draw the galaxy map
   --
   procedure draw_univ(w : window);
   --
   --  Draw the CAS window
   --
   procedure draw_cas(w : window);
   --
   --  Draw the planets window
   --
   procedure draw_planet(w : window);
   --
   --  Draw the enemies window
   --
   procedure draw_enemy(w : window);
private
   --
   --  Utility function to write sector position on screen
   --
   procedure sr_put_pos(p : data.sr_pos);
   --
   --  Draw a window frame and title
   --
   procedure frame(w : window; t : String);
end screen;
