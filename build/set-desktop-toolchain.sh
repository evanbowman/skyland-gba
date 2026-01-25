#!/bin/bash

rm -f CMakeCache.txt
rm -rf CMakeFiles/

cmake . -DGAMEBOY_ADVANCE=OFF
