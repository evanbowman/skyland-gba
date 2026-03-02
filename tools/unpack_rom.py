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


def load_symtab(raw_bytes):
    """Parse lisp_symtab.dat into a list of symbols ordered by index."""
    symbols = []
    text = raw_bytes.decode('utf-8')
    for line in text.splitlines():
        sym = line.rstrip('\0').strip()
        if sym:
            symbols.append(sym)
    return symbols


def restore_symbols(codestring, symbols):
    """Replace #N index references in lisp source with their original symbol names."""
    result = []
    i = 0
    while i < len(codestring):
        if codestring[i] == ';':
            # Comment: emit to end of line verbatim
            end = codestring.find('\n', i)
            if end == -1:
                result.append(codestring[i:])
                break
            result.append(codestring[i:end + 1])
            i = end + 1
        elif codestring[i] == '"':
            # String literal: emit verbatim, respecting escapes
            result.append('"')
            i += 1
            while i < len(codestring):
                ch = codestring[i]
                result.append(ch)
                if ch == '\\':
                    i += 1
                    if i < len(codestring):
                        result.append(codestring[i])
                        i += 1
                elif ch == '"':
                    i += 1
                    break
                else:
                    i += 1
        elif codestring[i] == '#':
            # Encoded symbol reference: read digits following '#'
            j = i + 1
            while j < len(codestring) and codestring[j].isdigit():
                j += 1
            if j > i + 1:
                idx = int(codestring[i + 1:j])
                if idx < len(symbols):
                    result.append(symbols[idx])
                else:
                    # Unknown index, emit as-is
                    result.append(codestring[i:j])
                i = j
            else:
                # Lone '#' with no digits, emit as-is
                result.append('#')
                i += 1
        else:
            result.append(codestring[i])
            i += 1
    return ''.join(result)


def iter_files(data):
    """Yield (path, raw_contents) for every file in the filesystem image."""
    pos = 0
    file_count = int.from_bytes(data[0:4], byteorder='little')
    pos += 4
    for _ in range(file_count):
        path = data[pos:pos + 62].strip(b'\0').decode('utf-8')[1:]  # strip leading slash
        pos += 62
        flags = int.from_bytes(data[pos:pos + 2], byteorder='little')
        pos += 2
        file_size = int.from_bytes(data[pos:pos + 4], byteorder='little')
        pos += 4
        padding = flags >> 13
        raw = data[pos:pos + file_size - (padding + 1)]  # +1 for null terminator
        pos += file_size
        yield path, raw


with open("Skyland.gba", 'rb') as f:
    s = f.read()

fs_loc = s.find(b'_FS_')
fs_loc += 4  # skip filesystem magic

with open("SkylandEngine.gba", "wb") as f:
    print("Extracting engine...")
    f.write(s[:fs_loc - 4])
    print("Wrote SkylandEngine.gba!")

data = s[fs_loc:]

with open("fs.extracted.bin", "wb") as f:
    print("Extracting filesystem...")
    f.write(data)

# Pass 1: find and load lisp_symtab.dat before extracting any lisp files
print("Loading symbol table...")
symbols = []
for path, raw in iter_files(data):
    if path == 'lisp_symtab.dat':
        symbols = load_symtab(raw)
        print("  Loaded {} symbols.".format(len(symbols)))
        break

if not symbols:
    print("  Warning: lisp_symtab.dat not found, symbol substitution will be skipped.")

# Pass 2: extract all files, restoring symbols in lisp sources
file_count = int.from_bytes(data[0:4], byteorder='little')
print("Extracting {} files...".format(file_count))

for path, raw in iter_files(data):
    print("  Extracting {}...".format(path))
    dirname = os.path.dirname(path)
    if dirname and not os.path.exists(dirname):
        os.makedirs(dirname)
    with open(path, "wb") as f:
        if path.split('.')[-1] == 'lisp':
            decoded = unminify_lisp(raw.decode('utf-8'))
            if symbols:
                decoded = restore_symbols(decoded, symbols)
            f.write(decoded.encode('utf-8'))
        else:
            f.write(raw)

print("Wrote files!")

# end of unpack_rom.py
