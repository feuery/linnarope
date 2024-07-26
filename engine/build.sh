#!/usr/bin/env zsh

set -euo pipefail

for cpp_file in src/*.cpp
do
    pathname=$cpp_file:t:r
    echo compiling $pathname
    clang++ $cpp_file -std=c++20 -g -O0 -c $(sdl2-config --cflags) -Iheaders -I/usr/local/include -I/opt/homebrew/Cellar/sdl2_image/2.8.2_1/include/ $(pkg-config --cflags sdl2_image)

    # -o finropedemo  -I$headers
done

clang++ *.o -o finropedemo $(sdl2-config --libs) -lpugixml $(pkg-config --libs sdl2_image)
