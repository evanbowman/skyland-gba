import os

# Unpacks the Skyland.gba rom, leaving the user with a SkylandEngine.gba rom,
# and the unpacked scripts. Intended for people who want to mod the game.
#
# To run, place this script in the same directory as Skyland.gba, and invoke the
# script with python version >= 3.2
#
# Then, you may run `python3 tools/encode_files.py` to repack the files into an
# archive, called fs.bin, which may be re-attached to the engine by running
# `cat SkylandEngine.gba fs.bin > SkylandMod.gba`.
#
# NOTE: If you're viewing this
# script in the SKYLAND text
# editor, and aren't sure how
# to get the unpack_rom.py
# script out of the rom: the
# file is stored in plain
# text, so you can simply open
# the Skyland.gba file in text
# mode in an editor and copy
# the script contents into a
# a local file.

def unminify_lisp(codestring):
    result = ""
    for c in codestring:
        if c == '\v':
            result += ' ' * 3
        elif c == '\t':
            result += ' ' * 4
        else:
            result += c
    return result


with open("Skyland.gba", 'rb') as f:
    s = f.read()

fs_loc = s.find(b'_FS_')
fs_loc += 4 # skip filesystem string

with open("SkylandEngine.gba", "wb") as f:
    print("Extracting engine...")
    f.write(s[:fs_loc-4])
    print("Wrote SkylandEngine.gba!")

with open("fs.extracted.bin", "wb") as f:
    print("Extracting filesystem...")
    data = s[fs_loc:]
    f.write(data)

    pos = 0

    file_count = int.from_bytes(data[0:4], byteorder='little')
    print("Extracting {} files...".format(file_count))

    pos += 4

    for f in range(0, file_count):
        path = data[pos:pos+62]
        path = path.strip(b'\0')
        path = path.decode("utf-8")[1:] # Ignore leading slash
        pos += 64 # (path field size)
        print("  Extracting {}...".format(path))

        file_size = int.from_bytes(data[pos:pos+4], byteorder='little')
        pos += 4

        dirname = os.path.dirname(path)

        if dirname == '':
            pass
        elif not os.path.exists(dirname):
            os.makedirs(dirname)

        with open(path, "wb") as f:
            contents = data[pos:pos+file_size-1] # -1 for null terminator
            if path.split('.')[-1] == "lisp":
                contents = unminify_lisp(contents.decode("utf-8")).encode('utf-8')
            f.write(contents)

        pos += file_size

    print("Wrote files!")

# end of unpack_rom.py
