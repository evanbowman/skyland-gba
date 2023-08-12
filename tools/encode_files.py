import sys
import os


project_root_path = os.path.split(os.path.dirname(os.path.realpath(__file__)))[0]

bytes_encoded = 0


def minify_lisp(codestring):
    result = ""
    within_comment = False
    within_string = False
    last_char = None

    for c in codestring:
        if within_comment:
            if c == '\n':
                within_comment = False
            continue

        if within_string:
            result += c
            if c == '"':
                within_string = False
            last_char = '"'
            continue

        if c == '"':
            result += c
            within_string = True
            continue

        if c == ';':
            within_comment = True
            continue

        if c == ' ' and (last_char == ' ' or last_char == None or last_char == ")"):
            # do nothing, we're collapsing spaces
            pass
        elif c == '\r':
            # don't care
            pass
        elif c == '\n':
            if last_char != ' ' and last_char != ")":
                result += ' '
                last_char = ' '
        else:
            result += c
            last_char = c

    return result


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

        if path.split('.')[-1] == 'lisp':
            file_contents = minify_lisp(data.decode('utf-8')).encode('utf-8')

        pad = 4 - (len(file_contents) + 1) % 4
        bytes_encoded += len(file_contents) + 1 + pad

        # +1 for null terminator
        out.write((len(file_contents) + 1 + pad).to_bytes(4, 'little'))

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

    fs_count = len(files_list)
    print("encoding %d files..." % fs_count)

    filesystem.write(fs_count.to_bytes(4, 'little'))

    for info in files_list:
        encode_file(info[1], info[0], filesystem)

    print("encoded {} bytes".format(bytes_encoded))

    print('done!')
