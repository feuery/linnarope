OBJECTS = $(patsubst src/%.cpp,%.o,$(wildcard src/*.cpp))
HEADERS := $(wildcard headers/*.h)

TEST_OBJS = $(patsubst test/src/%.cpp,%.o,$(wildcard test/src/*.cpp))
TEST_HEADERS := $(wildcard test/headers/*.h)

CFLAGS=-Wall -Werror -Wno-deprecated-declarations -std=c++20 -g -O0 -c -Iheaders -I/usr/local/include $$(pkg-config --cflags libpqxx)

LDFLAGS=$$(pkg-config --libs sqlite3) $$(pkg-config --libs libpqxx) $$(pkg-config libpq --libs) -L/usr/lib/


# DEPENDENCIES := libpqxx sqlite3  

exporter : $(OBJECTS)
	clang++ $(OBJECTS) -o exporter $(LDFLAGS)

$(OBJECTS): %.o: src/%.cpp $(HEADERS)
	clang++ $< $(CFLAGS)

.PHONY: clean
clean:
	rm $(OBJECTS) $(TEST_OBJS) exporter

# test-junit-gha: tests
# 	./exporter_test --gtest_output=xml:test-result.xml
