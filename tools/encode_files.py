import sys
import os


project_root_path = os.path.split(os.path.dirname(os.path.realpath(__file__)))[0]



def encode_file(path, real_name, out):
    with open(path) as test_file:
        encoded_path = real_name.encode('utf-8')

        if len(encoded_path) > 63:
            print("path %s too long" % real_name)
            sys.exit()

        out.write(encoded_path)

        for i in range(len(encoded_path), 64):
            out.write('\0'.encode('utf-8'))

        data = test_file.read()
        file_contents = data.encode('utf-8')

        # +1 for null terminator
        out.write((len(file_contents) + 1).to_bytes(4, 'little'))

        out.write(file_contents)
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

    files_list.append(["/readme.txt", os.path.join(project_root_path, "readme.txt")])

    fs_count = len(files_list)
    print("encoding %d files..." % fs_count)

    filesystem.write(fs_count.to_bytes(4, 'little'))

    for info in files_list:
        encode_file(info[1], info[0], filesystem)

    print('done!')
