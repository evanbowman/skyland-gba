# SKYLAND


## Overview

<img src="imgs_for_readme/header.png"/>

The production version of SKYLAND for gameboy advance.

<img src="imgs_for_readme/dialog.png"/>

<img src="imgs_for_readme/macro.png"/>

## Building

1) Install all of the standard devkitpro libgba stuff.
2) Install cmake
3) `cd build && ./set-gameboy-advance-toolchain.sh` (a shortcut for running cmake with the correct toolchain file)
4) run `make`
5) run `./bundle.sh`. The bundle script creates Skyland.gba by appending the game's scripts and resources to the compiled engine ROM.

## License

I have mixed feelings about GPL. I've never used a copy-left license like this. But I spent so much time on this project, and I think it's only fair that if someone else adds code to the project, they should share their changes too. If you want some code snippet or a few files under a more free license like BSD or MIT, then contact me.