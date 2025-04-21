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
package cas is
   --
   --  Enumeration for messages
   --
   type messages is (enemies, power, cmd_unknown, docked, no_dock, in_orbit,
                     no_orbit, no_torp, occupied, dest_self, dest_base, dest_planet,
                     dest_star, dest_enemy1, dest_empty, internal);
   --
   --  Enumeration for message priorities
   --
   type msg_prio is (alert, warning, info);
   --
   type msg is record
      text     : access constant String;
      priority : msg_prio;
      active   : Boolean;  --  Is the message active?
      once     : Boolean;  --  Display only once before making inactive?
   end record;
   --
   msg_list : array (messages) of msg;
   --
   procedure init;
   --
   function msg_text(m : messages) return String is (msg_list(m).text.all);
   --
   procedure set_msg(m : messages; p : msg_prio; o : Boolean);
   --
   procedure clear_msg(m : messages);
   --
   --  Clear all messages with "once" flag set.
   --
   procedure clear_once;
   --
private
   --
   --  Text for messages
   --
   msg_enemies     : aliased constant String := "Enemies detected in sector";
   msg_power       : aliased constant String := "Power is low";
   msg_cmd_unknown : aliased constant String := "Unknown command";
   msg_docked      : aliased constant String := "Docked to starbase";
   msg_no_dock     : aliased constant String := "Ship not adjacent to starbase";
   msg_in_orbit    : aliased constant String := "In orbit around a planet";
   msg_no_orbit    : aliased constant String := "Ship not adjacent to planet";
   msg_no_torp     : aliased constant String := "No torpedos to launch";
   msg_occupied    : aliased constant String := "Destination occupied";
   msg_dest_self   : aliased constant String := "You have destroyed yourself";
   msg_dest_base   : aliased constant String := "Starbase destroyed";
   msg_dest_planet : aliased constant String := "Planet destroyed";
   msg_dest_star   : aliased constant String := "Star destroyed";
   msg_dest_enemy1 : aliased constant String := "Enemy destroyed";
   msg_dest_empty  : aliased constant String := "A brief star shone in the blackness";
   msg_internal    : aliased constant String := "An internal program error occured";
end cas;
