# ANSITrek
A Game inspired by Super Star Trek and other similar vintage games.  It
takes advantage of the ANSIterm project to present an interface more similar
to trek52 on the PDP-11.

Since there are very few terminals left anymore the interface assumes
an adjustable ANSI style terminal interface with dimensions of 132x48
(the dimensions may change as the user interface gets developed).

This particular game is not as tied to Star Trek as some of the other versions.
The goal is to make much of the names configurable so that one could use it
to play in other universes besides Star Trek.

## Commands
The currently implemented commands are:
* LR - Long range scan
* JUMP - Move to a new sector
* MOVE - Move within a sector
* DOCK - Dock to a starbase (restores energy and torpedo count)
* ORBIT - Orbits a planet
* TORP - Fires a torpedo at the location destroying whatever is there
* QUIT - Exits the program
