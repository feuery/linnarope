#!/usr/bin/env zsh
headers=headers

for cpp_file in src/*.cpp
do
    pathname=$cpp_file:t:r
    echo compiling $pathname
    clang $cpp_file -g -O0 -c $(sdl2-config --cflags) -I$headers

    # -o finropedemo  -I$headers
done

clang *.o -o finropedemo $(sdl2-config --libs)
