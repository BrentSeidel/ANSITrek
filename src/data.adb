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
with cas;
package body data is
   --
   procedure init is
   begin
      rnd_star.Reset(g1);
      rnd_enem.Reset(g2);
      rnd_planet.Reset(g3);
      rnd_bool.Reset(g4);
      rnd_sect.Reset(g5);
      Ada.Numerics.Float_Random.Reset(g6);
      init_ship;
      init_lr;
      init_sr(ship.pos_lr.x, ship.pos_lr.y);
   end;
   --
   procedure init_lr is
      lr : lr_data;
   begin
      for i in universe_size'Range loop
         for j in universe_size'Range loop
            lr.stars   := rnd_star.Random(g1);
            lr.enemies := rnd_enem.Random(g2);
            lr.planets := rnd_planet.Random(g3);
            lr.base    := True;
            lr.destroyed := False;
            lr.discover  := False;
            u(i, j) := lr;
         end loop;
      end loop;
      u(ship.pos_lr.x, ship.pos_lr.y).discover := True;
   end;
   --
   procedure init_sr(x, y : universe_size) is
      x1 : sector_size := rnd_sect.Random(g5);
      y1 : sector_size := rnd_sect.Random(g5);
      index : Natural;
   begin
      --
      --  Initialize sector to empty
      for i in sector_size'Range loop
         for j in sector_size'Range loop
            sect(i, j) := empty;
         end loop;
      end loop;
      --
      --  Add stuff to sector
      --
      sect(ship.pos_sr.x, ship.pos_sr.y) := self;
      --
      for i in 1 .. u(x, y).stars loop
         while sect(x1, y1) /= empty loop
            x1 := rnd_sect.Random(g5);
            y1 := rnd_sect.Random(g5);
         end loop;
         sect(x1, y1) := star;
      end loop;
      --
      planet_count := u(x, y).planets;
      index := 1;
      for i in 1 .. u(x, y).planets loop
         while sect(x1, y1) /= empty loop
            x1 := rnd_sect.Random(g5);
            y1 := rnd_sect.Random(g5);
         end loop;
         sect(x1, y1) := planet;
         planets(index).pos := (x1, y1);
         planets(index).fuel := Natural(Ada.Numerics.Float_Random.Random(g6)*100.0);
         planets(index).destr := False;
         index := index + 1;
      end loop;
      --
      enemy_count := u(x, y).enemies;
      index := 1;
      for i in 1 .. u(x, y).enemies loop
         while sect(x1, y1) /= empty loop
            x1 := rnd_sect.Random(g5);
            y1 := rnd_sect.Random(g5);
         end loop;
         sect(x1, y1) := enemy1;
         enemies(index).pos := (x1, y1);
         enemies(index).energy := 100;
         enemies(index).destr := False;
         index := index + 1;
      end loop;
      --
      if u(x, y).base then
         while sect(x1, y1) /= empty loop
            x1 := rnd_sect.Random(g5);
            y1 := rnd_sect.Random(g5);
         end loop;
         sect(x1, y1) := base;
      end if;
   end;
   --
   procedure init_ship is
   begin
      ship.pos_lr.x := (universe_size'First + universe_size'Last) / 2;
      ship.pos_lr.y := (universe_size'First + universe_size'Last) / 2;
      ship.pos_sr.x := (sector_size'First + sector_size'Last) / 2 + 1;
      ship.pos_sr.y := (sector_size'First + sector_size'Last) / 2;
      ship.energy  := full_fuel;
      ship.shield  := 1000;
      ship.status  := green;
      ship.torpedo := full_torp;
      ship.elapsed := 0;
      ship.crew    := full_crew;
      ship.loc     := space;
   end;
   --
   --  Utility functions
   --
   --  Destroy the planet at the specified location
   --
   procedure dest_planet(p : sr_pos) is
      lr : data.lr_pos := data.ship.pos_lr;
   begin
      if sect(p.x, p.y) /= planet then
         cas.set_msg(cas.internal, cas.alert, True);
         return;
      end if;
      sect(p.x, p.y) := empty;
      cas.set_msg(cas.dest_planet, cas.alert, True);
      data.u(lr.x, lr.y).planets := data.u(lr.x, lr.y).planets - 1;
      for i in 1 .. planet_count loop
         if (planets(i).pos.x = p.x) and (planets(i).pos.y = p.y) then
            planets(i).destr := True;
            planets(i).fuel := 0;
            return;
         end if;
      end loop;
      cas.set_msg(cas.internal, cas.alert, True);
   end;
   --
   --  Destroy an enemy at the specified location
   --
   procedure dest_enemy(p : sr_pos) is
      lr : data.lr_pos := data.ship.pos_lr;
   begin
      if sect(p.x, p.y) /= enemy1 then
         cas.set_msg(cas.internal, cas.alert, True);
         return;
      end if;
      sect(p.x, p.y) := empty;
      cas.set_msg(cas.dest_enemy1, cas.alert, True);
      data.u(lr.x, lr.y).enemies := data.u(lr.x, lr.y).enemies - 1;
      for i in 1 .. enemy_count loop
         if (enemies(i).pos.x = p.x) and (enemies(i).pos.y = p.y) then
            enemies(i).destr := True;
            enemies(i).energy := 0;
            return;
         end if;
      end loop;
      cas.set_msg(cas.internal, cas.alert, True);
   end;
   --
end data;
