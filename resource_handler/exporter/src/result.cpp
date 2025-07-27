#include <cstdio>
#include <result.h>
#include <test_json.h>
#include <lib_fixup.h>

using json=nlohmann::json;

std::string tostring(AssertionMacro m) {
  switch(m) {
  case ensure:
    return "ENSURE";
  case ensure_throws:
    return "ENSURE_THROWS";
  case ensure_nothrow:
    return "ENSURE_NOTHROW";
  }
}

void Result::report(Reporter r) {
  switch(r){
  case HUMAN:
    printf("%s(%s) => %s\n", tostring(macro).c_str() , code.c_str(), result? "SUCCESS": "FAILURE");
    break;
  default:
    // wonder if anyone will ever call this path :D
    json j = this;
    auto dump = j.dump();
    spit(json_output_file().c_str(), dump);
    break;
  }
}
