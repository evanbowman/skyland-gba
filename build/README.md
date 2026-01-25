
For GAMEBOY builds:

The easiest way to set up a gba build is to run ./set-gameboy-advance-toolchain.sh or set-ninja-gameboy-advance-toolchain.sh. Then run make or ninja, depending on which toolchain you picked.


For macOS builds:

Simply run ./set-desktop-toolchain.sh. Then run make. The build system should produce a Skyland.app package, which you may double-click to run. The mac build expects the SDL2 and SDL2_image frameworks to be installed in /Library/Frameworks before building. Having the frameworks installed is necessary to produce a portable app package.

For linux builds:

Run ./set-desktop-toolchain.sh, then run make. I haven't spent much time on the linux port, to be honest. I got it running and then haven't bothered to do much work with it. I don't know how to distribute linux executable, to be honest...

For Windows builds:

Good luck! ...Be warned, I'm not a windows developer. I got the project to build, but I'm unsure how things are supposed to be done in windows. The CmakeLists.txt script expects SDL packages to be installed at the top level of the \c\ drive. Then, after building, please copy the necessary .dll files into the Release folder. The CMakeLists.txt build targets should already be copying all the other required resource files into the release folder.

I used visual studio 2019. In the settings, I remember needing to enable some option to tell visual studio to copy runtime dlls into the release folder, and also I needed to set the build type to release.

I included a .sln file in the repo, although I'm uncertain if anyone else will be able to use it. If not, I generated the solution by running:
```
cmake . -G "Visual Studio 16 2019" -A x64 -DGAMEBOY_ADVANCE=OFF
```
