#include <result.h>
#include <nlohmann/json.hpp>
#include <cassert>
#include <cstdio>
#include <test.h>
#include <test_json.h>
#include <lib_fixup.h>

using json=nlohmann::json;

Test::Test(std::function<bool(std::vector<Result>&)> test_fn, std::function<bool()> setup_fn,
           std::function<bool()> teardown_fn, std::string nme)
  :test_fn(test_fn), _setup(setup_fn), _teardown(teardown_fn), nme(nme) {}

bool Test::setup() { return _setup(); }
bool Test::teardown() { return _teardown(); }
void Test::run_test() {
  test_fn(results);
}

std::string& Test::name() { return nme; }

void Test::report(Reporter reporter) {
  assert(! results.empty());

  switch(reporter) {
  case HUMAN: 
    printf("TEST: %s\n", nme.c_str());

    for(auto& result: results) {
      result.report(reporter);
    }
    break;
  case JSON:
    json j = this;

    std::string dump = j.dump();

    spit(json_output_file().c_str(), dump);
    break;
  }
}

std::string json_output_file() {
  return appname + "-test-output.json";
}
