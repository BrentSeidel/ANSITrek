--
--  Author: Brent Seidel
--  Date: 16-Apr-2025
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
package body cas is
   --
   function msg_text(m : messages) return String is
   begin
      case m is
         when enemies =>
            return "Enemies detected in sector";
         when power =>
            return "Power is low";
         when cmd_unknown =>
            return "Unknown command";
         when docked =>
            return "Docked to starbase";
         when no_dock =>
            return "Ship not adjacent to starbase";
         when in_orbit =>
            return "In orbit around a planet";
         when no_orbit =>
            return "Ship not adjacent to planet";
         when no_torp =>
            return "No torpedos to launch";
         when occupied =>
            return "Destination occupied";
         when  dest_self =>
            return "You have destroyed yourself";
         when dest_base =>
            return "Starbase destroyed";
         when dest_planet =>
            return "Planet destroyed";
         when dest_star =>
            return "Star destroyed";
         when dest_enemy1 =>
            return "Enemy destroyed";
         when dest_empty =>
            return "A brief star shone in the blackness";
      end case;
   end;
   --
   procedure set_msg(m : messages; p : msg_prio; o : Boolean) is
     temp : constant msg := (priority => p, active => True, once => o);
   begin
      msg_list(m) := temp;
   end;
   --
   procedure clear_msg(m : messages) is
   begin
      msg_list(m).active := False;
   end;
   --
   --  Clear all messages with "once" flag set.
   --
   procedure clear_once is
   begin
      for m in messages loop
         if msg_list(m).once then
            msg_list(m).active := False;
            msg_list(m).once := False;
         end if;
      end loop;
   end;
   --
end cas;
