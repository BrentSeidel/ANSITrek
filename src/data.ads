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
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
package data is
   --
   --  This package contains the global data types and data for the ANSI Trek game.
   --
   type universe_size is range 1 .. 10;
   type sector_size is range 1 .. 10;
   --
   subtype num_stars  is Integer range 0 .. 9;
   subtype num_planet is Integer range 0 .. 9;
   subtype num_enemy  is Integer range 0 .. 9;
   --
   package rnd_sect is new Ada.Numerics.Discrete_Random(sector_size);
   package rnd_star is new Ada.Numerics.Discrete_Random(num_stars);
   package rnd_enem is new Ada.Numerics.Discrete_Random(num_enemy);
   package rnd_planet is new Ada.Numerics.Discrete_Random(num_planet);
   package rnd_bool is new Ada.Numerics.Discrete_Random(Boolean);
   --
   type lr_pos is record
      x : universe_size;
      y : universe_size;
   end record;
   --
   type sr_pos is record
      x : sector_size;
      y : sector_size;
   end record;
   --
   type lr_data is record
      stars     : Natural;
      enemies   : Natural;
      planets   : Natural;
      base      : Boolean;
      destroyed : Boolean;
      discover  : Boolean;
   end record;
   --
   type sr_data is (empty, star, base, enemy1, enemy2, enemy3, planet, self);
   --
   type lr_universe is array (universe_size, universe_size) of lr_data;
   type sr_sector is array (sector_size, sector_size) of sr_data;
   --
   type alert is (blue, green, red, yellow, cyan, magenta);
   type location is (docked, orbit, space);
   --
   full_fuel : Natural := 100000;
   full_torp : Natural := 20;
   type ship_state is record
      pos_lr  : lr_pos;
      pos_sr  : sr_pos;
      energy  : Natural;
      shield  : Natural;
      shields : Boolean;  --  Are shields up or down?
      status  : alert;
      torpedo : Natural;
      elapsed : Natural;
      loc     : Location;
   end record;
   --
   --  Main data structures
   --
   u : lr_universe;
   sect : sr_sector;
   ship : ship_state;
   --
   --  Common constants
   move_time : constant Natural := 1;
   move_energy : constant Natural := 1;
   jump_time : constant Natural := 10;
   jump_energy : constant Natural := 10;
   --
   --  Routines
   --
   procedure init;
   procedure init_lr;
   procedure init_sr(x, y : universe_size);
   procedure init_ship;
   --
private
   --
   --  Random number generator states
   --
   g1 : rnd_star.Generator;
   g2 : rnd_enem.Generator;
   g3 : rnd_planet.Generator;
   g4 : rnd_bool.Generator;
   g5 : rnd_sect.Generator;
   --
end data;
