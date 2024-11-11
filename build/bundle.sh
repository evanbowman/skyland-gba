
# NOTE: this isn't really necessary, just helpful when tracking down bugs in
# release builds.
git rev-parse --short HEAD > ../strings/commit_hash.txt

cp SkylandEngine Skyland.elf # (for mesen profiler)

python3 ../tools/encode_files.py
cat SkylandEngine.gba fs.bin > Skyland.gba
