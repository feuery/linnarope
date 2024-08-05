OBJECTS = $(patsubst src/%.cpp,%.o,$(wildcard src/*.cpp))
HEADERS := $(wildcard headers/*.h)

finropedemo : $(OBJECTS)
	clang++ $(OBJECTS) -o finropedemo $$(sdl2-config --libs) -lpugixml $$(pkg-config --libs sdl2_image)

# wonder if we could grep the dependent #include "headers.h" from the %.cpp
$(OBJECTS): %.o: src/%.cpp $(HEADERS)
	clang++ $< -std=c++20 -g -O0 -c $$(sdl2-config --cflags) -Iheaders -I/usr/local/include -I/opt/homebrew/Cellar/sdl2_image/2.8.2_1/include/ $$(pkg-config --cflags sdl2_image)

# $< contains the matched file 
