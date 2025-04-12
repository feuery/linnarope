OBJECTS = $(patsubst src/%.cpp,%.o,$(wildcard src/*.cpp))
HEADERS := $(wildcard headers/*.h)

# DEPENDENCIES := libpqxx sqlite3  

exporter : $(OBJECTS)
	clang++ $(OBJECTS) -o exporter $$(pkg-config --libs sqlite3) $$(pkg-config --libs libpqxx) $$(pkg-config libpq --libs)

$(OBJECTS): %.o: src/%.cpp $(HEADERS)
	clang++ $< -Wall -Werror -Wno-deprecated-declarations -std=c++20 -g -O0 -c -Iheaders -I/usr/local/include $$(pkg-config --cflags libpqxx)

.PHONY: clean
clean:
	find . -name '*.o' -delete; \
	rm exporter
