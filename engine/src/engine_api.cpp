#include <engine_api.h>
#include <cstdio>
#include <swank.h>

void register_callbacks(std::vector<fn> &fns) {
  for(auto fn: fns) {
    printf("Registering callback %s into lisp\n", fn.name);
    DEFUN(fn.name, fn.fun, fn.count_of_args);
  }
}
