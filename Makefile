OBJECTS = $(patsubst engine/src/%.cpp,%.o,$(wildcard engine/src/*.cpp))
HEADERS := $(wildcard engine/headers/*.h)

TEST_OBJS = $(patsubst engine/test/src/%.cpp,%.o,$(wildcard engine/test/src/*.cpp))
TEST_HEADERS := $(wildcard engine/test/headers/*.h)
CFLAGS=-Wall -Werror -std=c++20 -g -O0 -c $$(sdl2-config --cflags) -Iengine/headers -I/usr/local/include $$(pkg-config --cflags SDL2_image) $$(ecl-config --cflags) $$(pkg-config nlohmann_json --cflags) $$(pkg-config --cflags SDL2_ttf)

LDFLAGS=$$(sdl2-config --libs) -lpugixml $$(pkg-config --libs SDL2_image) $$(pkg-config --libs sqlite3) $$(ecl-config --libs) $$(pkg-config --libs SDL2_ttf)

TEST_CFLAGS=$(CFLAGS) $$(pkg-config catch2 --cflags)
TEST_LDFLAGS=$$(pkg-config catch2 --libs) -lCatch2Main $(LDFLAGS)

finropedemo : $(OBJECTS) exporter
	clang++ $(OBJECTS) -o finropedemo $(LDFLAGS)

# wonder if we could grep the dependent #include "headers.h" from the %.cpp
$(OBJECTS): %.o: engine/src/%.cpp $(HEADERS)
	clang++ $< $(CFLAGS)

# $< contains the matched file

# builds the test binary 
tests: $(TEST_OBJS) $(OBJECTS)
	clang++ $(TEST_LDFLAGS) $(TEST_OBJS) $(filter-out main.o, $(OBJECTS)) -o finropedemotests

# runs tests
.PHONY: test
test: tests
	./finropedemotests

test-junit-gha: tests
	./finropedemotests --reporter JUnit::out=result-junit.xml

$(TEST_OBJS): %.o: engine/test/src/%.cpp $(TEST_HEADERS)
	clang++ $< $(TEST_CFLAGS)

# install dependencies
.PHONY: deps
deps:
	./install_dependencies.sh

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
