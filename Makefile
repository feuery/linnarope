OBJECTS = $(patsubst engine/src/%.cpp,%.o,$(wildcard engine/src/*.cpp))
HEADERS := $(wildcard engine/headers/*.h)

finropedemo : $(OBJECTS) exporter
	clang++ $(OBJECTS) -o finropedemo $$(sdl2-config --libs) -lpugixml $$(pkg-config --libs sdl2_image) $$(pkg-config --libs sqlite3) $$(ecl-config --libs) $$(pkg-config --libs sdl2_ttf)

# wonder if we could grep the dependent #include "headers.h" from the %.cpp
$(OBJECTS): %.o: engine/src/%.cpp $(HEADERS)
	clang++ $< -Wall -Werror -std=c++20 -g -O0 -c $$(sdl2-config --cflags) -Iengine/headers -I/usr/local/include -I/opt/homebrew/Cellar/sdl2_image/2.8.2_1/include/ $$(pkg-config --cflags sdl2_image) $$(ecl-config --cflags) $$(pkg-config nlohmann_json --cflags) $$(pkg-config --cflags sdl2_ttf)

# $< contains the matched file 

.PHONY: clean
clean:
	rm $(OBJECTS) finropedemo

# resource manager specific tasks

.PHONY: exporter
exporter:
	$(MAKE) -C ./resource_handler/exporter

## runs resource manager
.PHONY: resource-manager
resource-manager: exporter
	sbcl --load ./resource_handler/resource-handler.asd --eval '(asdf:make "linnarope-resource-handler")' --eval '(linnarope.main:start-server)'

example-tetris: finropedemo
	./finropedemo --game ./examples/tetris-scripts/linnarope-export.game --import-scripts-from ./examples/tetris-scripts
	echo Ran import
	./finropedemo --game ./examples/tetris-scripts/linnarope-export.game 
