# Bytecode

For better performance, and more compactness, some of the lisp scripts are
pre-compiled to bytecode packages. To compile a bytecode ackage, you can use the
build-library function provided in stdlib.lisp. For portability, you need to
build libraries with relocatable linkage.


# Compiling lisp scripts

To recompile the bytecode modules in this directory, use the desktop build of
the game, navigate to the build directory in the Skyland git repository, and
run: ./rebuild-packages-mac.sh
NOTE: this script only works for macOS. You'll need to implement it differently
for other operating systems.

Then, you'll want to rebuild whichever edition of the game you're looking to
recompile.

If you are modding the game and struggling to rebuild packages manually, you
could instead load the packages sources directly.
For example, by replacing this line in your init.lisp file:
(load-library "/scripts/packages/core.slb")
with:
(eval-script "/scripts/packages/source/core.lisp")
You can just make edits to the library source and load it directly.
Or, if you want to improve startup times a bit by caching the results in save
memory:
(load-library-cached "/scripts/packages/source/core.lisp"
                     "/bytecode/core.slb")
