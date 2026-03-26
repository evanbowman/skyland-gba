# Bytecode

For better performance, and more compactness, some of the lisp scripts are
pre-compiled to bytecode packages. To compile a bytecode ackage, you can use the
build-library function provided in stdlib.lisp. For portability, you need to
build libraries with relocatable linkage.

NOTE: Moving compiled .slb object files to the rom filesystem is still partly
aspirational. It seems to work, but needs more testing.


# Compiling lisp scripts

To recompile the bytecode modules in this directory, use the desktop build of
the game, navigate to the build directory in the Skyland git repository, and
run: ./rebuild-packages-mac.sh
NOTE: this script only works for macOS. You'll need to implement it differently
for other operating systems.

Then, you'll want to rebuild whichever edition of the game you're looking to
recompile.
