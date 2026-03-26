cp -r ../scripts Skyland.app/Contents/Resources/
./Skyland.app/Contents/MacOS/Skyland --compile-packages --no-window-system
mkdir extract-packages
cp /Users/evanbowman/Library/Application\ Support/Skyland/save.dat ./extract-packages/Skyland.sav
cp ../tools/unpack_sav.py ./extract-packages
cd extract-packages
python3 unpack_sav.py
cp extracted_cart_ram/scripts/packages/*.slb ../../scripts/packages/
