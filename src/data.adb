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
with screen;
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
      enemies_killed := 0;
      planets_destr  := 0;
      bases_destr    := 0;
   end;
   --
   procedure init_lr is
      lr : lr_data;
   begin
      enemies_remain := 0;
      for i in galaxy_size'Range loop
         for j in galaxy_size'Range loop
            lr.stars   := rnd_star.Random(g1);
            lr.enemies := rnd_enem.Random(g2);
            enemies_remain := enemies_remain + lr.enemies;
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
   procedure init_sr(x, y : galaxy_size) is
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
         planets(index).pos     := (x1, y1);
         planets(index).fuel    := Natural(Ada.Numerics.Float_Random.Random(g6)*100.0);
         planets(index).destr   := False;
         planets(index).scanned := False;
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
         enemies(index).pos    := (x1, y1);
         enemies(index).energy := 1000;
         enemies(index).destr  := False;
         enemies(index).shot   := 0;
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
      ship.pos_lr.x := (galaxy_size'First + galaxy_size'Last) / 2;
      ship.pos_lr.y := (galaxy_size'First + galaxy_size'Last) / 2;
      ship.pos_sr.x := (sector_size'First + sector_size'Last) / 2 + 1;
      ship.pos_sr.y := (sector_size'First + sector_size'Last) / 2;
      ship.energy  := full_fuel;
      ship.shield  := 1000;
      ship.status  := green;
      ship.torpedo := full_torp;
      ship.elapsed := 0;
      ship.crew    := full_crew;
      ship.loc     := space;
      ship.orbit   := 0;
   end;
   --
   --  Utility functions
   --
   --  Attack the planet at the specified location
   --
   procedure attack_planet(p : sr_pos; e : Natural) is
      lr : lr_pos := ship.pos_lr;
      sr : sr_pos := ship.pos_sr;
   begin
      if sect(p.x, p.y) /= planet then
         cas.set_msg(cas.internal, cas.alert, True);
         return;
      end if;
      --
      --  Search for target
      --
      for i in 1 .. planet_count loop
         if (planets(i).pos.x = p.x) and (planets(i).pos.y = p.y) then
            sect(p.x, p.y) := empty;
            cas.set_msg(cas.dest_planet, cas.alert, True);
            u(lr.x, lr.y).planets := u(lr.x, lr.y).planets - 1;
            planets(i).destr := True;
            planets(i).fuel := 0;
            planets_destr := planets_destr + 1;
            --
            --  Check if ship is in orbit around this planet.
            --
            if (ship.orbit = i) then
               ship.orbit := 0;
               ship.loc   := space;
               screen.draw_msg("You destroyed the planet that you're orbiting!");
            end if;
            return;
         end if;
      end loop;
      cas.set_msg(cas.internal, cas.alert, True);
   end;
   --
   --  Attack an enemy at the specified location
   --
   procedure attack_enemy(p : sr_pos; e : Natural) is
      lr : lr_pos := ship.pos_lr;
   begin
      if sect(p.x, p.y) /= enemy1 then
         cas.set_msg(cas.internal, cas.alert, True);
         return;
      end if;
      --
      --  Search for target
      --
      for i in 1 .. enemy_count loop
         if (enemies(i).pos.x = p.x) and (enemies(i).pos.y = p.y) then
            if e >= enemies(i).energy then
               cas.set_msg(cas.dest_enemy1, cas.alert, True);
               data.u(lr.x, lr.y).enemies := data.u(lr.x, lr.y).enemies - 1;
               enemies_remain := enemies_remain - 1;
               enemies_killed := enemies_killed + 1;
               sect(p.x, p.y) := empty;
               enemies(i).destr := True;
               enemies(i).energy := 0;
            else
               cas.set_msg(cas.target_hit, cas.info, True);
               enemies(i).energy := enemies(i).energy - e;
            end if;
            return;
         end if;
      end loop;
      cas.set_msg(cas.internal, cas.alert, True);
   end;
   --
   --  Compute attackes from all enemies on ship
   --
   procedure attack_ship is
      hit : Natural;
   begin
      for e of enemies loop
         --
         --  Destroyed enemies can't attack
         --
         if not e.destr then
            hit := Integer(Ada.Numerics.Float_Random.Random(g6)*Float(e.energy)*0.8);
            e.energy := e.energy - hit;
            e.shot := hit;
            if hit > 0 then
               --
               --  Check for shields and modify hit strenght
               --
               if ship.shields then
                  if hit > ship.shield then
                     hit := hit / 2;
                  else
                     hit := hit / 10;
                  end if;
                  if hit > ship.shield then
                     ship.shield  := 0;
                     ship.shields := False;
                  else
                     ship.shield := ship.shield - 1;
                     hit := 0;
                  end if;
               end if;
               if hit > ship.energy then
                  ship.energy := 0;
               else
                  ship.energy := ship.energy - hit;
               end if;
               cas.set_msg(cas.attack, cas.alert, True);
            end if;
         end if;
      end loop;
   end;
   --
   --  Find items
   --
   function adjacent_planet(p : sr_pos; c : out Natural) return item_list is
      count  : Natural := 0;
--      dist_x : Integer;
--      dist_y : Integer;
      list   : item_list := (others => 0);
   begin
      for i in 1 .. planet_count loop
--         dist_x := Integer(p.x) - Integer(planets(i).pos.x);
--         dist_y := Integer(p.y) - Integer(planets(i).pos.y);
--         if (not planets(i).destr) and ((abs dist_x) <= 1) and ((abs dist_y) <= 1) then
         if (not planets(i).destr) and (dist2(p, planets(i).pos) <= 2) then
            count := count + 1;
            list(count) := i;
         end if;
      end loop;
      c := count;
      return list;
   end;
   --
   --  Check if ship is adjacent to object of type o.  Mostly used to see if ship
   --  is next to a starbase for docking.
   --
   function check_adjacent(o : sr_data) return Boolean is
      pos : sr_pos := ship.pos_sr;
   begin
      if pos.x > sector_size'First then
         if pos.y > sector_size'First then
            if sect(pos.x - 1, pos.y - 1) = o then
               return True;
            end if;
         end if;
         if sect(pos.x - 1, pos.y) = o then
           return True;
         end if;
         if pos.y < sector_size'Last then
            if sect(pos.x - 1, pos.y + 1) = o then
               return True;
            end if;
         end if;
      end if;
      if pos.y > sector_size'First then
         if sect(pos.x, pos.y - 1) = o then
            return True;
         end if;
      end if;
      if pos.y < sector_size'Last then
         if sect(pos.x, pos.y + 1) = o then
            return True;
         end if;
      end if;
      if pos.x < sector_size'Last then
         if pos.y > sector_size'First then
            if sect(pos.x + 1, pos.y - 1) = o then
               return True;
            end if;
         end if;
         if sect(pos.x + 1, pos.y) = o then
            return True;
         end if;
         if pos.y < sector_size'Last then
            if sect(pos.x + 1, pos.y + 1) = o then
               return True;
            end if;
         end if;
      end if;
      return False;
   end;
   --
   --  Compute distance squared between two points.  Points must be both lr_pos
   --  or sr_pos.
   --
   --  Have to use Integer because p1.x - p2.x or p1.y - p2.y might be negative.
   --  The square of the difference will be positive, so the return value can be
   --  Natural.
   --
   function dist2(p1, p2 : sr_pos) return Natural is
      dist_x : Integer := Integer(p1.x) - Integer(p2.x);
      dist_y : Integer := Integer(p1.y) - Integer(p2.y);
   begin
      return Natural(dist_x*dist_x + dist_y*dist_y);
   end;
   --
   function dist2(p1, p2 : lr_pos) return Natural is
      dist_x : Integer := Integer(p1.x) - Integer(p2.x);
      dist_y : Integer := Integer(p1.y) - Integer(p2.y);
   begin
      return Natural(dist_x*dist_x + dist_y*dist_y);
   end;
   --
end data;
