#include <cstdio>
#include <result.h>

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
    puts("Not implemented");
    break;
  }
}
