
         SKYLAND


 Welcome to the SKYLAND
 source code!

 NOTE: this readme is
 intended to be displayed on
 a gba screen when running
 Skyland in developer mode,
 hence the narrow formatting.

 If you simply want to edit
 scripts and rebuild the rom
 using the modding toolchain,
 run the following commands
 after editing script files simply run repack.sh.

 If you have the Skyland rom
 but not the engine rom or the
 scripts, download the
 unpack_rom.py script, place
 it in the same directory as
 Skyland.gba, and run
 unpack_rom.py.

 If you only have the Skyland
 rom and not the unpack_rom.py
 script, then you may extract
 the unpack_rom.py file from
 the Skyland.gba rom by
 opening the rom in a text
 editor, searching for the
 filename, and copy-pasting
 it to another file (all tools
 appended to the skyland
 engine are encoded in plain-
 text).

 The below info is intended
 for those who want to edit
 scripts on the gba itself,
 using the developer mode
 setting:

 The rom/ filesystem
 contains all of the game's
 level scripts.

 You may either:

 1) create new scripts in
 the sram filesystem, and
 load them in
 mods/init.lisp

 2) simply edit scripts in
 the rom filesystem, and
 the engine will write your
 modified scripts to the
 sram filesystem.

 You cannot edit rom
 scripts directly, of
 course, as the files
 live in read only memory.

 But SKYLAND preferentially
 loads files from sram
 before attempting to load
 them from rom, so if you
 create an sram file with an
 identical name and path
 to a file in rom, you can
 effectively override
 the rom script.

 If you're just getting
 started, try playing
 around with some of the
 code in /scripts/config/

 Afterwards, maybe you'll
 want to edit newgame.lisp
 or other such files.

 You'll find the level
 scripts in the events/
 folder, organized by
 zone.

 If you manage to mess
 anything up and break the
 game, simply open the file
 browser and delete the
 problematic file in sram/
 (by selecting the file,
 pressing start, and then
 choosing the delete
 option).

 One final note: the game
 will not run any custom
 code when in multiplayer
 mode. Keeping games sync'd
 up when people are running
 different code would just
 be too difficult.
