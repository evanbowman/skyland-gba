#!/bin/bash

rm CMakeCache.txt
rm -r CMakeFiles/

cmake -DNINTENDO_DS=ON -DGBA_AUTOBUILD_IMG=ON -DGAMEBOY_ADVANCE=OFF -DCMAKE_TOOLCHAIN_FILE=$(pwd)/devkitarm.cmake .
