OBJECTS = $(patsubst engine/src/%.cpp,%.o,$(wildcard engine/src/*.cpp))
HEADERS := $(wildcard engine/headers/*.h)

CFLAGS=-Wall -Werror -std=c++20 -g -O0 -c $$(sdl2-config --cflags) -Iengine/headers -I/usr/local/include $$(pkg-config --cflags SDL2_image) $$(ecl-config --cflags) $$(pkg-config nlohmann_json --cflags) $$(pkg-config --cflags SDL2_ttf)

LDFLAGS=$$(sdl2-config --libs) -lpugixml $$(pkg-config --libs SDL2_image) $$(pkg-config --libs sqlite3) $$(ecl-config --libs) $$(pkg-config --libs SDL2_ttf) -L/usr/lib/

# TEST_FLAGS=-j
TEST_FLAGS=
TEST_COMPILER_DEPS=$(wildcard test-compiler/*.fs) test-compiler/test-compiler.fsproj
TEST_COMPILER_PATH=test-compiler/test-compiler/test-compiler

finropedemo : $(OBJECTS) exporter
	clang++ $(OBJECTS) -o finropedemo $(LDFLAGS)

# wonder if we could grep the dependent #include "headers.h" from the %.cpp
$(OBJECTS): %.o: engine/src/%.cpp $(HEADERS)
	clang++ $< $(CFLAGS)

# $< contains the matched file

# runs tests

.PHONY: clean-test-files
clean-test-files:
	rm -f *-test-output.json *-test-output.json.xml  # Without -f this would fail and stop running the makefile in a clean repo 

.PHONY: test
test: finropedemo test-compiler clean-test-files
	./finropedemo --run-tests $(TEST_FLAGS)
	./resource_handler/exporter/exporter -test $(TEST_FLAGS) 
	find . -name '*test-output.json' -exec $(TEST_COMPILER_PATH) --input {} --output {}.xml \;

$(TEST_COMPILER_PATH): $(TEST_COMPILER_DEPS)
	$(MAKE) -C test-compiler

.PHONY: test-compiler
test-compiler: $(TEST_COMPILER_PATH)

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
