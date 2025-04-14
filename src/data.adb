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
with Ada.Numerics.Float_Random;
package body data is
   --
   procedure init is
   begin
      ship.pos_lr.x := (universe_size'First + universe_size'Last) / 2;
      ship.pos_lr.y := (universe_size'First + universe_size'Last) / 2;
      ship.pos_sr.x := (sector_size'First + sector_size'Last) / 2;
      ship.pos_sr.y := (sector_size'First + sector_size'Last) / 2;
      ship.energy  := 100000;
      ship.shields := 1000;
      ship.status  := green;
      lr_init;
   end;
   --
   procedure lr_init is
      lr : lr_data;
   begin
      for i in universe_size'Range loop
         for j in universe_size'Range loop
            lr.stars   := 1;
            lr.enemies := 1;
            lr.planets := 1;
            lr.base    := True;
            lr.destroyed := False;
            u(i, j) := lr;
         end loop;
      end loop;
   end;
end data;
