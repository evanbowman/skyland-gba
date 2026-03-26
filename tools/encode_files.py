import sys
import os
import struct
import zlib


project_root_path = os.path.split(os.path.dirname(os.path.realpath(__file__)))[0]


bytes_encoded = 0


lisp_symbol_tab = {}
lisp_constant_tab = {}


def build_lisp_constant_tab(codestring):
    i = 0
    while i < len(codestring):
        start = codestring.find("(defconstant ", i)
        if start == -1:
            break
        # Skip past "(defconstant "
        i = start + 13
        while i < len(codestring) and codestring[i].isspace():
            i += 1
        sym_start = i
        while i < len(codestring) and not codestring[i].isspace():
            i += 1
        symbol = codestring[sym_start:i]
        while i < len(codestring) and codestring[i].isspace():
            i += 1
        # Extract constant value (handle expressions with parens)
        const_start = i
        if i < len(codestring) and codestring[i] == '"':
            # Handle quoted strings
            i += 1
            while i < len(codestring) and codestring[i] != '"':
                i += 1
            if i < len(codestring):
                i += 1
        elif i < len(codestring) and codestring[i] == '(':
            # Handle parenthesized expressions
            paren_count = 0
            while i < len(codestring):
                if codestring[i] == '(':
                    paren_count += 1
                elif codestring[i] == ')':
                    paren_count -= 1
                    if paren_count == 0:
                        i += 1
                        break
                i += 1
        else:
            # Handle unquoted atomic values
            while i < len(codestring) and not codestring[i].isspace() and codestring[i] != ')':
                i += 1
        constant = codestring[const_start:i]
        if symbol and constant:
            lisp_constant_tab[symbol] = constant


def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

# This is just an optimization allowing us to minimize the size of the lisp string
# intern table. Otherwise, the interpreter we would need to store variable names
# permanently in memory.
def build_lisp_symtab(codestring):
    newstr = ""
    inquotes = False
    incomment = False
    for c in codestring:
        if incomment:
            if c == '\n':
                incomment = False
                newstr = newstr + c
        elif inquotes:
            if c == '"':
                inquotes = False
        else:
            if c == ';':
                incomment = True
            elif c == '"':
                inquotes = True
            else:
                newstr = newstr + c

    newstr = newstr.replace('(', ' ')
    newstr = newstr.replace(')', ' ')
    newstr = newstr.replace("'", ' ')
    newstr = newstr.replace(',', ' ')
    newstr = newstr.replace('@', ' ')
    newstr = newstr.replace("\n", ' ')
    parsed = newstr.split(' ')
    for string in parsed:
        if ';' in string:
            break
        if not is_number(string.replace('/', '0').lstrip('-')) and not '"' in string and string and not string.startswith("0x") and string != "false" and string != "true":
            if len(string) > 3: # The interpreter does a small string optimization already
                if string in lisp_symbol_tab:
                    lisp_symbol_tab[string] = lisp_symbol_tab[string] + 1
                else:
                    lisp_symbol_tab[string] = 1


def build_symbol_index_map():
    """Build a fast lookup dict mapping symbol_str -> '#N' replacement string.
    Called once after all files have been preprocessed."""
    result = {}
    for i, (sym, _) in enumerate(sorted(lisp_symbol_tab.items(), key=lambda item: item[0])):
        result[sym] = "#{}".format(i)
    return result


def replace_symbols_in_code(code, index_map):
    """Replace whole symbol tokens in a code region (no strings, no comments)."""
    result = []
    i = 0
    while i < len(code):
        c = code[i]
        if c in '() \t\n\r,\'@':
            result.append(c)
            i += 1
        else:
            # Collect a token
            j = i
            while j < len(code) and code[j] not in '() \t\n\r,\'@':
                j += 1
            token = code[i:j]
            replacement = index_map.get(token)
            if replacement is not None:
                result.append(replacement)
            else:
                result.append(token)
            i = j
    return ''.join(result)


def replace_symbols(contents, index_map):
    """Replace symbols in a lisp source file, correctly handling string
    literals and comments."""
    decoded = contents.decode('utf-8')
    result = []
    i = 0
    while i < len(decoded):
        # Find the next string literal or comment start
        str_pos = decoded.find('"', i)
        cmt_pos = decoded.find(';', i)

        # Pick whichever comes first
        next_special = min(
            str_pos if str_pos != -1 else len(decoded),
            cmt_pos if cmt_pos != -1 else len(decoded)
        )

        # Everything up to next_special is plain code — replace symbols
        if next_special > i:
            result.append(replace_symbols_in_code(decoded[i:next_special], index_map))

        if next_special == len(decoded):
            break

        if decoded[next_special] == ';':
            # Comment: emit everything up to and including the newline verbatim
            end = decoded.find('\n', next_special)
            if end == -1:
                result.append(decoded[next_special:])
                i = len(decoded)
            else:
                result.append(decoded[next_special:end + 1])
                i = end + 1
        else:
            # String literal: find the closing quote, respecting escapes
            result.append('"')
            i = next_special + 1
            while i < len(decoded):
                ch = decoded[i]
                result.append(ch)
                if ch == '\\':
                    i += 1
                    if i < len(decoded):
                        result.append(decoded[i])
                        i += 1
                elif ch == '"':
                    i += 1
                    break
                else:
                    i += 1

    return ''.join(result).encode('utf-8')


def extract_slb_symbols(path):
    """Read a .slb (Skyland Lisp Bytecode) object file and return the list of
    symbol strings from its relocation table.  Returns an empty list for
    non-relocatable files or on any parse error."""
    try:
        with open(path, 'rb') as f:
            data = f.read()
    except OSError:
        return []

    # Header: flags(2) + fingerprint(8) + definition_count(2) = 12 bytes
    if len(data) < 12:
        return []

    flags = struct.unpack_from('<H', data, 0)[0]
    relocatable = bool(flags & 1)
    if not relocatable:
        return []

    # RelocationTableInfo immediately follows the header: offset(4)
    if len(data) < 16:
        return []

    rtab_offset = struct.unpack_from('<I', data, 12)[0]
    if rtab_offset >= len(data):
        return []

    # The relocation table is a sequence of null-terminated strings
    symbols = []
    rtab = data[rtab_offset:]
    current = bytearray()
    for b in rtab:
        if b == 0:
            if current:
                symbols.append(current.decode('utf-8', errors='replace'))
            current = bytearray()
        else:
            current.append(b)
    # A trailing non-null-terminated entry (shouldn't happen, but be safe)
    if current:
        symbols.append(current.decode('utf-8', errors='replace'))

    return symbols


def extension(path):
    return path.split('.')[-1]



def make_index_file(path):
    index = 0
    with open(path.split('.')[0] + ".idx", 'wb') as index_file:
        with open(path, 'rb') as test_file:
            b = test_file.read(1)
            while b != b"":
                index = index + 1
                if b == "\n".encode():
                    index_file.write(index.to_bytes(4, 'little'))
                b = test_file.read(1)


fs_hash = None


def preprocess_file(path, real_name, out):
    global fs_hash
    with open(path, 'rb') as test_file:
        data = test_file.read()
        if extension(path) == 'lisp':
            src_contents = data.decode('utf-8')
            build_lisp_symtab(src_contents)
            build_lisp_constant_tab(src_contents)
        elif extension(path) == 'slb':
            for sym in extract_slb_symbols(path):
                if sym in lisp_symbol_tab:
                    lisp_symbol_tab[sym] = lisp_symbol_tab[sym] + 1
                else:
                    lisp_symbol_tab[sym] = 1
        if not fs_hash:
            fs_hash = zlib.crc32(data)
        else:
            fs_hash = zlib.crc32(data, fs_hash)


def encode_file(path, real_name, out, symbol_index_map=None):

    with open(path, 'rb') as test_file:

        global bytes_encoded

        if bytes_encoded % 4 != 0:
            print('invalid padding!?', bytes_encoded % 4)

        encoded_path = real_name.encode('utf-8')

        if len(encoded_path) > 61:
            print("path %s too long" % real_name)
            sys.exit()

        out.write(encoded_path)

        for i in range(len(encoded_path), 62):
            out.write('\0'.encode('utf-8'))

        bytes_encoded += 64

        data = test_file.read()
        file_contents = data

        if extension(path) == 'lisp':
            file_contents = replace_symbols(file_contents, symbol_index_map or {})

        null_padding = 1

        if len(file_contents) == 0:
            raise Exception("empty file " + path)

        if file_contents[-1] == '\0':
            null_padding = 0

        pad = 4 - (len(file_contents) + null_padding) % 4
        bytes_encoded += len(file_contents) + null_padding + pad

        # Two reserved bytes for file data flags
        out.write((pad << 13).to_bytes(2, 'little'))

        # +1 for null terminator
        out.write((len(file_contents) + null_padding + pad).to_bytes(4, 'little'))

        out.write(file_contents)
        out.write('\0'.encode('utf-8'))

        for i in range(0, pad):
            out.write('\0'.encode('utf-8'))


def is_emacs_backup_file(name):
    return '~' in name or '#' in name or '.DS_Store' in name


def collect_paths(paths_list, subdir):
    for root, dirs, files in os.walk(os.path.join(project_root_path, subdir), topdown=False):
        if dirs:
            dirs.sort()
        if files:
            files.sort()

        for sdir in dirs:
            collect_paths(paths_list, subdir + '/' + sdir)
        for name in files:
            if os.path.basename(root) == subdir.split('/')[-1]:
                full = os.path.join('/' + subdir, name)
                if not is_emacs_backup_file(full):
                    paths_list.append(['/' + subdir + '/' + name, os.path.join(root, name)])



with open('fs.bin', 'wb') as filesystem:
    print("creating fs.bin...")

    filesystem.write("_FS_".encode('utf-8'))

    files_list = []

    collect_paths(files_list, "strings")
    collect_paths(files_list, "scripts")
    collect_paths(files_list, "help")
    collect_paths(files_list, "misc")
    collect_paths(files_list, "licenses")
    collect_paths(files_list, "tools")
    collect_paths(files_list, "data")

    files_list.append(["/readme.txt", os.path.join(project_root_path, "readme.txt")])
    files_list.append(["/repack.sh", os.path.join(project_root_path, "repack.sh")])
    files_list.append(["/boot.ini", os.path.join(project_root_path, "boot.ini")])

    for info in files_list:
        if extension(info[0]) == "idf":
            make_index_file(info[1])

    fs_count = len(files_list) + 3 # +1 for symtab file, +1 for constant tab, +1 for filesystem hash
    print("encoding %d files..." % fs_count)

    filesystem.write(fs_count.to_bytes(4, 'little'))

    # Pass 1: scan all files to build the symbol and constant tables
    for info in files_list:
        preprocess_file(info[1], info[0], filesystem)

    # Build fast index map once, after all files have been scanned
    symbol_index_map = build_symbol_index_map()

    symtab_fname = "lisp_symtab.dat"
    symtab_path = os.path.join(project_root_path, symtab_fname)
    with open(symtab_path, 'wb') as sym_file:
        for sym, v in sorted(lisp_symbol_tab.items(), key=lambda item: item[0]):
            enc_sym = sym.encode('utf-8')
            fs_hash = zlib.crc32(enc_sym, fs_hash)
            if len(enc_sym) > 30:
                raise Exception("symbol " + sym + " too long!")
            sym_file.write(sym.encode('utf-8'))
            for i in range(len(enc_sym), 31):
                sym_file.write('\0'.encode('utf-8'))
            sym_file.write('\n'.encode('utf-8')) # This is just to make the file easier to read in an editor. Yes, it wastes space.

    fs_hash_fname = "fs_hash.dat"
    fs_hash_path = os.path.join(project_root_path, fs_hash_fname)
    with open(fs_hash_path, 'wb') as fs_hash_file:
        fs_hash_file.write(struct.pack('l', fs_hash))

    encode_file(fs_hash_path, "/" + fs_hash_fname, filesystem)

    # Pass 2: encode all files with symbol replacement
    for info in files_list:
        encode_file(info[1], info[0], filesystem, symbol_index_map)

    encode_file(symtab_path, "/" + symtab_fname, filesystem)
    print("symbol tab count: %d" % len(lisp_symbol_tab))

    consttab_fname = "lisp_constant_tab.dat"
    consttab_path = os.path.join(project_root_path, consttab_fname)
    with open(consttab_path, 'wb') as const_file:
        for sym, v in lisp_constant_tab.items():
            enc_sym = sym.encode('utf-8')
            enc_constant = v.encode('utf-8')
            const_file.write((len(enc_sym) + 1).to_bytes(1, 'little'))
            const_file.write((len(enc_constant) + 1).to_bytes(1, 'little'))
            const_file.write(enc_sym)
            const_file.write('\0'.encode('utf-8'))
            const_file.write(enc_constant)
            const_file.write('\0'.encode('utf-8'))
            if len(enc_constant) > 255:
                raise Exception("constant value " + str(enc_constant) + " for constant " + str(enc_sym) + " exceeds 255 bytes!")

    encode_file(consttab_path, "/" + consttab_fname, filesystem)
    print("constant tab count: %d" % len(lisp_constant_tab))

    print("encoded {} bytes".format(bytes_encoded))

    print('done!')
