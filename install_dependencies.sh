#!/bin/sh

if command -v brew >/dev/null 2>&1
then
    # We're probably on a Mac
    brew install sdl2 sdl2_gfx sdl2_image sdl2_ttf ecl nlohmann-json pugixml sqlite libpq libpqxx    
elif command -v pacman >/dev/null 2>&1
then
    # We're probably on an Arch Linux
    pacman -S sdl2_gfx sdl2_image sdl2_ttf ecl nlohmann-json pugixml sqlite libpqxx postgresql-libs sdl3 sdl2-compat # Arch seems to not package sdl2 anymore
    
else
    echo "Unknown package manager/os. Currently known are brew and pacman. Pull requests welcome?"
fi
