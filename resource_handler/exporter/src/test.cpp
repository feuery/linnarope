#include "result.h"
#include <cassert>
#include <test.h>

Test::Test(std::function<bool(std::vector<Result>&)> test_fn, std::function<bool()> setup_fn,
           std::function<bool()> teardown_fn, std::string nme)
  : reporter(HUMAN), test_fn(test_fn), _setup(setup_fn), _teardown(teardown_fn), nme(nme) {}

bool Test::setup() { return _setup(); }
bool Test::teardown() { return _teardown(); }
void Test::run_test() {
  test_fn(results);
}

std::string& Test::name() { return nme; }

void Test::report() {
  assert(! results.empty());

  if (reporter == HUMAN) {
    printf("TEST: %s\n", nme.c_str());
  }

  for(auto& result: results) {
    result.report(reporter);
  }
}
  
