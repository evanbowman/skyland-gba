# SKYLAND


## Overview

<img src="imgs_for_readme/header.png"/>

The production version of SKYLAND for gameboy advance.

<img src="imgs_for_readme/dialog.png"/>

<img src="imgs_for_readme/macro.png"/>


## Code Quality

Ok, let's just get this out of the way now :) I originally built Skyland in 20
days for a game jam, and parts of the code are a bit rough. Through beta
testing, deadlines, bugfixes, etc., Skyland, accumulated some bad
code. Ultimately, this is a video game, and everything must be in service to the
user experience. I keep things just modular enough that I'm able to easily
develop new features without too much friction.

I think sometimes the free software crowd burns way too much time on writing,
rewriting code many times over and not enough time thinking about how their
programs actually feel in the hands of the users. Resulting in a plethora of
soulless linux gui applications (I say this as a longtime linux user).

tldr; Singlehandedly writing a large production quality game on a system with
less than 300kb of memory is quite hard. Sometimes one has to cut corners to
ship something while maintaining one's health and sanity.


## Building

1) Install all of the standard devkitpro libgba stuff.
2) Install cmake
3) `cd build && ./set-gameboy-advance-toolchain.sh` (a shortcut for running cmake with the correct toolchain file)
4) run `make`
5) run `./bundle.sh`. The bundle script creates Skyland.gba by appending the game's scripts and resources to the compiled engine ROM.

## License

I have mixed feelings about GPL. I've never used a copy-left license like this. But I spent so much time on this project, and I think it's only fair that if someone else adds code to the project, they should share their changes too. If you want some code snippet or a few files under a more free license like BSD or MIT, then contact me.