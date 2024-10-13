OBJECTS = $(patsubst engine/src/%.cpp,%.o,$(wildcard engine/src/*.cpp))
HEADERS := $(wildcard engine/headers/*.h)

finropedemo : $(OBJECTS)
	clang++ $(OBJECTS) -o finropedemo $$(sdl2-config --libs) -lpugixml $$(pkg-config --libs sdl2_image) $$(pkg-config --libs sqlite3)

# wonder if we could grep the dependent #include "headers.h" from the %.cpp
$(OBJECTS): %.o: engine/src/%.cpp $(HEADERS)
	clang++ $< -Wall -Werror -std=c++20 -g -O0 -c $$(sdl2-config --cflags) -Iengine/headers -I/usr/local/include -I/opt/homebrew/Cellar/sdl2_image/2.8.2_1/include/ $$(pkg-config --cflags sdl2_image)

# $< contains the matched file 

.PHONY: clean
clean:
	find . -name '*.o' -delete; \
	rm finropedemo
