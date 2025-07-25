OBJECTS = $(patsubst engine/src/%.cpp,%.o,$(wildcard engine/src/*.cpp))
HEADERS := $(wildcard engine/headers/*.h)

CFLAGS=-Wall -Werror -std=c++20 -g -O0 -c $$(sdl2-config --cflags) -Iengine/headers -I/usr/local/include $$(pkg-config --cflags SDL2_image) $$(ecl-config --cflags) $$(pkg-config nlohmann_json --cflags) $$(pkg-config --cflags SDL2_ttf)

LDFLAGS=$$(sdl2-config --libs) -lpugixml $$(pkg-config --libs SDL2_image) $$(pkg-config --libs sqlite3) $$(ecl-config --libs) $$(pkg-config --libs SDL2_ttf) -L/usr/lib/

finropedemo : $(OBJECTS) exporter
	clang++ $(OBJECTS) -o finropedemo $(LDFLAGS)

# wonder if we could grep the dependent #include "headers.h" from the %.cpp
$(OBJECTS): %.o: engine/src/%.cpp $(HEADERS)
	clang++ $< $(CFLAGS)

# $< contains the matched file

# runs tests
.PHONY: test
test: finropedemo
	./finropedemo --run-tests && ./resource_handler/exporter/exporter -test

# .PHONY: test-junit-gha
# test-junit-gha: test
# 	./resource_handler/exporter/exporter_test --gtest_output=xml:exporter-result-junit.xml


# install dependencies
.PHONY: deps
deps:
	./install_dependencies.sh

.PHONY: clean
clean:
	rm $(OBJECTS) finropedemo
	$(MAKE) -C ./resource_handler/exporter clean

# resource manager specific tasks

.PHONY: exporter
exporter:
	$(MAKE) -C ./resource_handler/exporter

.PHONY: exporter-test
exporter-test: exporter
	./resource_handler/exporter/exporter -test

## runs resource manager
.PHONY: resource-manager
resource-manager: exporter
	sbcl --load ./resource_handler/resource-handler.asd --eval '(asdf:make "linnarope-resource-handler")' --eval '(linnarope.main:start-server)'

example-tetris: finropedemo
	./finropedemo --game ./examples/tetris-scripts/linnarope-export.game --import-scripts-from ./examples/tetris-scripts
	echo Ran import
	./finropedemo --game ./examples/tetris-scripts/linnarope-export.game 
