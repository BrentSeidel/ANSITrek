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
package data is
   --
   --  This package contains the global data types and data for the ANSI Trek game.
   --
   type lr_data is record
      stars     : Natural;
      enemies   : Natural;
      planets   : Natural;
      base      : Boolean;
      destroyed : Boolean;
   end record;
   --
   type sr_data is (empty, star, base, enemy1, enemy2, enemy3, planet);
end data;
