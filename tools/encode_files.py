import sys
import os


project_root_path = os.path.split(os.path.dirname(os.path.realpath(__file__)))[0]

bytes_encoded = 0


def minify_lisp(codestring):
    # originally I was actually minifying the sources.
    # But spaces to tabs alone saves a lot of space... without the
    # hassle of having to unminify stuff when I unzip the rom.

    result = ""
    spc_count = 0

    for c in codestring:
        if c == ' ':
            spc_count += 1
            if spc_count == 4:
                result += '\t'
                spc_count = 0
        else:
            if spc_count:
                result += ' ' * spc_count
                spc_count = 0

            result += c

    if spc_count:
        result += ' ' * spc_count
        spc_count = 0

    # We're doing a little hack to replace 3 spaces with a vertical tab. The
    # interpreter ignores whitespace, and doing this saves rom space.

    result2 = ""
    for c in result:
        if c == ' ':
            spc_count += 1
            if spc_count == 3:
                result2 += '\v'
                spc_count = 0
        else:
            if spc_count:
                result2 += ' ' * spc_count
                spc_count = 0

            result2 += c

    if spc_count:
        result2 += ' ' * spc_count

    return result2



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



def encode_file(path, real_name, out):

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

        # Two reserved bytes for file data flags
        out.write('\0'.encode('utf-8'))
        out.write('\0'.encode('utf-8'))

        bytes_encoded += 64

        data = test_file.read()
        file_contents = data

        if extension(path) == 'lisp':
            file_contents = minify_lisp(data.decode('utf-8')).encode('utf-8')

        null_padding = 1

        if len(file_contents) == 0:
            raise Exception("empty file " + path)

        if file_contents[-1] == '\0':
            null_padding = 0

        pad = 4 - (len(file_contents) + null_padding) % 4
        bytes_encoded += len(file_contents) + null_padding + pad

        # +1 for null terminator
        out.write((len(file_contents) + null_padding + pad).to_bytes(4, 'little'))

        out.write(file_contents)
        out.write('\0'.encode('utf-8'))

        for i in range(0, pad):
            out.write('\0'.encode('utf-8'))



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
                if not '~' in full:
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
    files_list.append(["/boot.ini", os.path.join(project_root_path, "boot.ini")])

    for info in files_list:
        if extension(info[0]) == "idf":
            make_index_file(info[1])

    fs_count = len(files_list)
    print("encoding %d files..." % fs_count)

    filesystem.write(fs_count.to_bytes(4, 'little'))

    for info in files_list:
        encode_file(info[1], info[0], filesystem)

    print("encoded {} bytes".format(bytes_encoded))

    print('done!')
