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
   type messages is (enemies, power, cmd_unknown, docked, no_dock, in_orbit,
                     no_orbit, no_torp, occupied, dest_self, dest_base, dest_planet,
                    dest_star, dest_enemy1, dest_empty);
   type msg_prio is (alert, warning, info);
   --
   type msg is record
      priority : msg_prio;
      active   : Boolean;  --  Is the message active?
      once     : Boolean;  --  Display only once before making inactive?
   end record;
   --
   msg_list : array (messages) of msg;
   --
   function msg_text(m : messages) return String;
   --
   procedure set_msg(m : messages; p : msg_prio; o : Boolean);
   --
   procedure clear_msg(m : messages);
   --
   --  Clear all messages with "once" flag set.
   --
   procedure clear_once;
   --
end cas;
