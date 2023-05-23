# SKYLAND


## Overview

<img src="imgs_for_readme/header.png"/>

The production version of SKYLAND for gameboy advance.

<img src="imgs_for_readme/dialog.png"/>

<img src="imgs_for_readme/macro.png"/>


## Building

1) Install all of the standard devkitpro libgba stuff.
2) Make sure you have python3 and the PIP image library
3) Install cmake
4) `cd build && ./set-gameboy-advance-toolchain.sh` (a shortcut for running cmake with the correct toolchain file)
5) run ./build.sh

## License

All artwork and music assets are proprietary.
I'm providing the source code itself under the terms of the GPL license. If you want some code snippet or a few files under a more permissive license like BSD or MIT, then contact me.

## Coding Standards

This codebase grew out of a rushed game jam project. Some aspects of the code, particularly older parts of the codebase, shouldn't be considered great examples of good programming style. Furthermore, the GBA is a 20+ year old embedded system, and it's hard to keap code neat for a project of this size.
