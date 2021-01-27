# la-gridfield

Role-playing games often include a "field system" in which the player moves
an avatar around a space representing a field, dungeon, or city.  In older field
systems this space is a two-dimensional grid: in general everything exists at
one of a finite set of grid points.

This package provides a basic grid-based field system.  The terrain is a set
of tiles at specific grid points, and characters move from one grid point to
another in discrete movements.  The movements are animated between the points to
improve the visual effect. They are driven by keyboard input, in the case of the
avatar, and task scripts, in the case of non-player characters (NPCs).

A primary advantage of the grid is that it provides an efficient structure for
looking up information about the vicinity of a character.  For example, to
check whether a character can move forward, we only need to check whether it
can occupy the grid point in front of it instead of, say, comparing its
location with the locations of all other obstacles.

The avatar can inspect the grid point in front of it, querying the point for a
cue for how to respond.  This cue may be, for example, to begin displaying some
prose.  Similarly, the avatar looks for a different cue from each grid point
that it traverses.  Such a cue might be to move, for example, from the current
outdoor map to a particular indoor map.  Other cues triggered by inspection
and traversal are possible, but the stock avatar implementation does not react
to them.
