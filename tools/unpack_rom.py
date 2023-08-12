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

def fmt_lisp_let(expr, indent):
    result = expr[0]
    result += " "
    result += "("
    for b in expr[1]:
        result += fmt_lisp_sexpr(b, indent)
    result += ")"
    tab = indent + 2
    for e in expr[2:]:
        result += os.linesep + (" " * tab) + fmt_lisp_atom(e, tab)
    return result

def fmt_lisp_if(expr, indent):
    result = expr[0]
    result += " "
    result += fmt_lisp_atom(expr[1], indent)
    if len(expr[2:]) > 1:
        tab = indent + 4
        result += os.linesep + (" " * tab) + fmt_lisp_atom(expr[2], tab)
        tab -= 2;
        result += os.linesep + (" " * tab) + fmt_lisp_atom(expr[3], tab)
    else:
        tab = indent + 2
        result += os.linesep + (" " * tab) + fmt_lisp_atom(expr[2], tab)
    return result

def fmt_lisp_defn(expr, indent):
    result = str(expr[0])
    tab = indent + 2
    result += " " + str(expr[1])
    for atom in expr[2:]:
        result += os.linesep + (" " * tab) + fmt_lisp_atom(atom, tab)
    return result

def fmt_lisp_atom(atom, indent):
    if type(atom) == list:
        return fmt_lisp_sexpr(atom, indent)
    else:
        return str(atom)

def fmt_lisp_sexpr(expr, indent):
    result = "("
    tab = indent
    if len(expr):
        if expr[0] == "defn" or expr[0] == "defn/c": result += fmt_lisp_defn(expr, indent)
        elif expr[0] == "let": result += fmt_lisp_let(expr, indent)
        elif expr[0] == "if": result += fmt_lisp_if(expr, indent)
        else:
            z = True
            for atom in expr:
                if not z:
                    result += " "
                result += fmt_lisp_atom(atom, tab)
                z = False
    result += ")"
    return result

def fmt_lisp_syntax(tree):
    result = ""
    for entry in tree:
        result += fmt_lisp_sexpr(entry, 0)
        result += os.linesep + os.linesep
    return result

def read_lisp_toplevel(code):
    result = []
    off = 0
    while True:
        if off == len(code) or code[off] == '\0':
            break
        elif code[off] == '(':
            off += 1
            nested, d = read_lisp_sexpr(code, off)
            off += d
            result.append(nested)
        else:
            off += 1
    return result

def read_lisp_sexpr(code, offset):
    result = []
    off = 0
    word = ""
    while True:
        if code[off + offset] == '(':
            if len(word):
                result.append(word)
                word = ""
            off += 1
            nested, d = read_lisp_sexpr(code, off + offset)
            off += d
            result.append(nested)
        elif code[off + offset] == " ":
            if len(word):
                result.append(word)
                word = ""
            off += 1
        elif code[off + offset] == ")":
            if len(word):
                result.append(word)
            return result, off + 1
        elif code[off + offset] == '"':
            if len(word):
                result.append(word)
            word += '"'
            off += 1
            while code[off + offset] != '"':
                word += code[off + offset]
                off += 1
            word += '"'
            off += 1
        else:
            word += code[off + offset]
            off += 1


def unminify_lisp(codestring):
    result = ";;; unminified by unpack_rom.py" + os.linesep
    result += ";;; sorry about the formatting! feel free to improve it..." + os.linesep * 2
    result += fmt_lisp_syntax(read_lisp_toplevel(codestring))
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
