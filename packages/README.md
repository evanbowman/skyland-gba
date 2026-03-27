# Bytecode

For better performance, and more compactness, some of the lisp scripts are
pre-compiled to bytecode packages. To compile a bytecode package, you can use
the build-library function provided in stdlib.lisp. For portability, you need to
build libraries with relocatable linkage.


# Compiling lisp scripts

To recompile the bytecode modules in this directory, use the desktop build of
the game, and run:
./Skyland --compile-packages=../packages/source --output=../packages/bin --no-window-system

NOTE: in the macOS app, for example, you can find the executable in:
./Skyland.app/Contents/MacOS/

If you are modding the game and struggling to rebuild packages manually, you
could instead load the packages sources directly.
For example, by replacing this line in your init.lisp file:
(load-library "/packages/lib/core.slb")
with:
(eval-script "/packages/source/core.lisp")
