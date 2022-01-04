# SKYLAND


## Overview

<img src="imgs_for_readme/header.png"/>

The production version of SKYLAND for gameboy advance.


<img src="imgs_for_readme/skyland-fight-x2.gif"/>


## Building

1) Install all of the standard devkitpro libgba stuff.
2) Install cmake
3) `cd build && ./set-gameboy-advance-toolchain.sh` (a shortcut for running cmake with the correct toolchain file)
4) run `make`
5) run `./bundle.sh`. The bundle script creates Skyland.gba by appending the game's scripts and resources to the compiled engine ROM.
