OBJECTS = $(patsubst src/%.cpp,%.o,$(wildcard src/*.cpp))
HEADERS := $(wildcard headers/*.h)

TEST_OBJS = $(patsubst test/src/%.cpp,%.o,$(wildcard test/src/*.cpp))
TEST_HEADERS := $(wildcard test/headers/*.h)

CFLAGS=-Wall -Werror -Wno-deprecated-declarations -std=c++20 -g -O0 -c -Iheaders -I/usr/local/include $$(pkg-config --cflags libpqxx)

LDFLAGS=$$(pkg-config --libs sqlite3) $$(pkg-config --libs libpqxx) $$(pkg-config libpq --libs) -L/usr/lib/

TEST_CFLAGS=$(CFLAGS) $$(pkg-config catch2 --cflags) -I./test/headers
TEST_LDFLAGS=$$(pkg-config catch2-with-main --libs) $(LDFLAGS)


# DEPENDENCIES := libpqxx sqlite3  

exporter : $(OBJECTS)
	clang++ $(OBJECTS) -o exporter $(LDFLAGS)

$(OBJECTS): %.o: src/%.cpp $(HEADERS)
	clang++ $< $(CFLAGS)

.PHONY: clean
clean:
	rm $(OBJECTS) $(TEST_OBJS) exporter

# builds the test binary

$(TEST_OBJS): %.o: test/src/%.cpp $(TEST_HEADERS)
	clang++ $< $(TEST_CFLAGS)

tests: $(TEST_OBJS) $(OBJECTS)
	clang++ $(TEST_LDFLAGS) $(TEST_OBJS) $(filter-out main.o, $(OBJECTS)) -o exporter_test

# runs tests
.PHONY: test
test: tests
	./exporter_test

test-junit-gha: tests
	./exporter_test --reporter JUnit::out=result-junit.xml
