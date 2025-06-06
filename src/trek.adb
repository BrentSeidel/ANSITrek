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
--
with Ada.Text_IO;
with BBS.ANSI;
with cas;
with cli;
with data;
with screen;
procedure trek is
begin
   data.init;
   screen.init;
   cli.init;
   cas.init;
   cli.cmds;
   --
   --  Print ending statistics
   --
   Ada.Text_IO.Put_Line("Total enemies destroyed: " & Natural'Image(data.enemies_killed));
   Ada.Text_IO.Put_Line("Total planets attacked:  " & Natural'Image(data.planets_destr));
   Ada.Text_IO.Put_Line("Total bases destroyed:   " & Natural'Image(data.bases_destr));
   Ada.Text_IO.Put_Line(BBS.ANSI.rst);
end;
