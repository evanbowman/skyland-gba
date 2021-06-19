# Skyland


## Overview

A Gameboy Advance game created, frantically, for a game jam, in about twenty five
days. For posterity, I will not be editing this repository after finishing the
game jam. All future work will be completed in a fork (link forthcoming).

I forked a top-down action game called Blind Jump to leverage some existing gba
framework code that I'd written in the past for interfacing with the hardware,
but most of the code is entirely new. For game-specific code, see the source/skyland 
directory.


## Controls, Mechanics, and other Info

Skyland has nuanced controls and mechanics, which I will describe below.


### Islands

All of the action in the game takes place on flying castles. At most two islands
comprise each level, where the player controls one island, and an AI controls
the other island. Hostile fortresses will attack the player, while neutral or
peaceful islands may give the player coins, items, or blocks.


### Rooms

Each Island consists of a layer of terrain, atop of which the player can build
structures. The game generalizes all structures as rooms, although not all rooms
can be occupied (players cannot move characters into solid rooms, like cannons
or walls, for instance).

To build new rooms, open up the construction menu by pressing the right
bumper. You may then press A to select a construction site, and then scroll
through a list of structures to build with the B button. Press A again to
construct the selected room. The game may reject your request to build
structures, if you have not collected enough coins, or have not constructed
enough power cores to sustain the new addition.

See below for a complete annotation of all structions that you may build:

#### Hull

The simplest block in the game. A purely defensive structure. The only structure
in the game that does not supply or deplete energy.

#### Cannon

For attacking other islands. Mainly used for attacking from left-to-right.

#### Missile Silo

For attacking other islands. Missiles will launch vertically, and fall down
vertically on the other island. For attacking the top face of an island.

#### Stairwell

A tall room, with no function other than to allow your fortress' residents to
move between blocks stacked on top of one another.

#### Workshop

A square room. Slightly lowers the cost of building all rooms, and allows you to
build the more complex structures listed below.

#### Power Core

Your island will have one power core at the beginning of the game. If a castle's
power core is destroyed, it will sink into the clouds, and all of the island's
residents will perish. You must have a workshop in your castle to build a Power
Core.

#### Forcefield

Almost identical to a Hull block, but consumes power, and allows your own
weapons to shoot through the forcefields without damageing them. Mainly intended
for protecting weapons (otherwise, you could just build Hull blocks). You must
have a workshop to build a Forcefield.

#### Radar

Allows you to see the interior of your opponent's structures. Fragile, and
consumes a relatively large amount of power. You must have a workshop to build a
Radar.
