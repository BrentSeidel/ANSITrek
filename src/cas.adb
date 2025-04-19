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
   --  Attach the messages to the message array.
   --
   procedure init is
   begin
      msg_list(enemies).text     := msg_enemies'Access;
      msg_list(power).text       := msg_power'Access;
      msg_list(cmd_unknown).text := msg_cmd_unknown'Access;
      msg_list(docked).text      := msg_docked'Access;
      msg_list(no_dock).text     := msg_no_dock'Access;
      msg_list(in_orbit).text    := msg_in_orbit'Access;
      msg_list(no_orbit).text    := msg_no_orbit'Access;
      msg_list(no_torp).text     := msg_no_torp'Access;
      msg_list(occupied).text    := msg_occupied'Access;
      msg_list(dest_self).text   := msg_dest_self'Access;
      msg_list(dest_base).text   := msg_dest_base'Access;
      msg_list(dest_planet).text := msg_dest_planet'Access;
      msg_list(dest_star).text   := msg_dest_star'Access;
      msg_list(dest_enemy1).text := msg_dest_enemy1'Access;
      msg_list(dest_empty).text  := msg_dest_empty'Access;
   end;
   --
   procedure set_msg(m : messages; p : msg_prio; o : Boolean) is
   begin
      msg_list(m).priority := p;
      msg_list(m).active := True;
      msg_list(m).once := o;
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
