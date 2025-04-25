#pragma once

#include <vector>
#include <ecl/ecl.h>

class fn {
public:
  const char *name;
  cl_objectfn_fixed fun;
  int count_of_args;
};

void register_callbacks(std::vector<fn> &fns);
