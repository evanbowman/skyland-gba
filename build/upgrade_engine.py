import os

with open("Skyland.gba", 'rb') as f:
    s = f.read()

fs_loc = s.find(b'_FS_')

fs = s[fs_loc:]

with open("Skyland.gba", 'wb') as out:
    with open("SkylandEngine.gba", 'rb') as engine:
        out.write(engine.read())
        out.write(fs)
