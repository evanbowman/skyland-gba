# Skyland


## Overview

<img src="imgs_for_readme/header.png"/>

A Gameboy Advance game created, frantically, for a game jam, in about twenty five
days. For posterity, I will not be editing this repository after finishing the
game jam. All future work will be completed in a fork (link forthcoming).

I forked a top-down action game called Blind Jump to leverage some existing gba
framework code that I'd written in the past for interfacing with the hardware,
but most of the code is entirely new. For game-specific code, see the source/skyland 
directory, and the scripts directory, where you'll find all the various level scenarios
as LISP code.

<img src="imgs_for_readme/skyland-fight-x2.gif"/>


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
through a list of structures to build with the d-pad. Press A again to
construct the selected room. The game may reject your request to build
structures, if you have not collected enough coins, or have not constructed
enough power cores to sustain the new addition.

You may also salvage and recover some value from existing rooms, by selecting
the room with the cursor, and pressing A.

You may pause the game to make adjustments at any time, by pressing the left
bumper. Pausing frequently does affect your highscore a bit though.

See below for a complete annotation of all structures that you may build:

---

#### Hull

<img src="imgs_for_readme/hull.png"/>

The simplest block in the game. A purely defensive structure. The only structure
in the game that does not supply or deplete energy.

---

#### Cannon

<img src="imgs_for_readme/cannon.png"/>

For attacking other islands. Mainly used for attacking horizontally. Select a 
cannon and press A to choose a target. By default, the selector will move between
targets sequentially (left to right, top to bottom), but you can tap the R bumper
while choosing a target to detach and move the selector without constraint.

---

#### Missile Silo

<img src="imgs_for_readme/missile-silo.png"/>

For attacking other islands. Missiles will launch vertically, and fall down
vertically on the other island. For attacking the top face of an island.

---

#### Stairwell

<img src="imgs_for_readme/stairwell.png"/>

A tall room, with no function other than to allow your fortress' residents to
move between blocks stacked on top of one another.

---

#### Infirmary

<img src="imgs_for_readme/infirmary.png"/>

Heals each idle occupant _controlled by the owner of the infirmary._
Your characters will not heal while fighting an enemy. The health
provided by the infrirmary will be distributed evenly across all occupants, so a
player will heal more quickly if other characters are not attempting to heal at
the same time.

---

### Bulkhead

<img src="imgs_for_readme/bulkhead.png"/>

Select a bulkhead and press A to open/close. Characters cannot walk through closed
bulkhead doors. Use to limit invading enemy movement within your castle.

---

#### Workshop

<img src="imgs_for_readme/workshop.png"/>

A square room. Slightly lowers the cost of building all rooms, and allows you to
build the more complex structures listed below.

---

#### Power Core

<img src="imgs_for_readme/power-core.png"/>

Your island will include one power core at the beginning of the game. If all of a
castle's power cores are destroyed, it will sink into the clouds, and all of the
island's residents will perish. You must have a workshop in your castle to build
aadditional Power Cores. Each room on your island consumes an amount of power (with the
sole exception of hull blocks, which consume no power). If the total power
consumption of your island exceeds the combined power output of your power
cores, all systems will shut down, until you salvage rooms to free up power, or
until you build more power cores.

---

#### Forcefield

<img src="imgs_for_readme/forcefield.png"/>

Almost identical to a Hull block, but consumes power, and your own weapons may
shoot through forcefield blocks without damageing them. Mainly intended for
protecting weapons (otherwise, you could just build Hull blocks). You must have
a workshop to build a Forcefield.

---

#### Transporter

<img src="imgs_for_readme/transporter.png"/>

Allows you to transport one of your characters to another island. Move one of
your characters into the transporter, select the transporter, and press the
A button. Your character will be transported to a
random room in the opponent's island. Transporters have a cool-down period, so
you may need to build multiple transporters if you want to effectively raid
enemy islands. By selecting an empty transporter, you may also retrieve
characters on other islands. A light on the transporter tile image will change
from red to blue when the transporter finishes recharging.

---

#### Radar

<img src="imgs_for_readme/radar.png"/>

Allows you to see the interior of your opponent's structures. Fragile, and
consumes a relatively large amount of power. You must have a workshop to build a
Radar.

---

#### Ion Cannon

<img src="imgs_for_readme/ion-cannon.png"/>

An energy weapon. Fires projectiles that pass through hull and rooms. Deals
significant damage to forcefields.

---

#### Replicator

<img src="imgs_for_readme/replicator.png"/>

Produces replicants. Move one of your human characters into the replicator, select
the replicator, and press A; the game will be give you the option to create a
replicant. Replicants cannot heal in an infirmary, and have 75% of the health of
the template character from which they were created. Under the hood, using
replicants affects your highscore calculation, but this doesn't matter too much
if you're just playing for fun.

---

### Characters

Characters, such as your castle's residents, or enemy goblins, can damage or
repair rooms. When successfully destroying another castle's room, a character
will plunder a fraction of the room's construction cost, increasing your coin
count.


## Hacking

You can find all of the different level scenarios specified as lisp scripts, in
the scripts directory. If you open up one of the release ROMs, find the `_FS_`
string (near the end of the ROM), and delete the symbol, along with everything
after it, you will have a clean ROM file, containing only the engine. Now, make
whatever changes that you'd like to the game's scripts. Next, you can run the
encode_files.py script in the build/ directory, which produces a filesystem,
stored in fs.bin. You may then cat together the filesystem and the engine ROM,
to produce a runnable game.
