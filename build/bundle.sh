#
# bundle.sh
#

# NOTE: this isn't really necessary, just helpful when tracking down bugs in
# release builds.
git rev-parse --short HEAD > ../strings/commit_hash.txt
cp SkylandEngine Skyland.elf # (for mesen profiler)

# Compile lisp bytecode packages using the desktop build, if available.
SKYLAND_BIN=""
if [ -x "Skyland.app/Contents/MacOS/Skyland" ]; then
    python3 ../tools/encode_files.py
    cp -r ../scripts Skyland.app/Contents/Resources
    cp ../lisp_symtab.dat Skyland.app/Contents/Resources
    SKYLAND_BIN="Skyland.app/Contents/MacOS/Skyland"
elif [ -x "Skyland.exe" ]; then
    SKYLAND_BIN="./Skyland.exe"
elif [ -x "Skyland" ]; then
    SKYLAND_BIN="./Skyland"
fi

if [ -n "$SKYLAND_BIN" ]; then
    echo "Compiling packages with $SKYLAND_BIN..."
    "$SKYLAND_BIN" --compile-packages=../packages/source --output=../packages/lib --no-window-system #--compile-verbose
else
    echo "WARNING: No desktop build found in the current directory."
    echo "Bytecode packages in /script/packages/ will not be recompiled."
    echo "To compile them, build the desktop edition and place it here,"
    echo "then re-run this script."
    echo "Rebuilding the bytecode packages isn't strictly necessary."
fi

python3 ../tools/encode_files.py
cat SkylandEngine.gba fs.bin > Skyland.gba
