#include <test.h>

Test::Test(std::function<bool()> test_fn, std::function<bool()> setup_fn,
           std::function<bool()> teardown_fn, std::string nme)
  : test_fn(test_fn), _setup(setup_fn), _teardown(teardown_fn), nme(nme) {}

bool Test::setup() { return _setup(); }
bool Test::teardown() { return _teardown(); }
Result Test::run_test() {
  return test_fn()?
    Result { SUCCESS, "example code"}:
    Result { FAILURE, "example code"};
}

std::string& Test::name() { return nme; }
