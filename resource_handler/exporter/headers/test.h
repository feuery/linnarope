#pragma once

/**
 * A homegrown automatic test assertion system.
 *
 * Written because trying to link googletest or
 * catch2 in github actions / archlinux:base-devel
 * was a disaster in the summer of 2025.
 */

#include "result.h"
#include <functional>

class Test {
private:
  std::function<bool()> test_fn, _setup, _teardown;

  std::string nme;
  
public:

  Test(std::function<bool()> test_fn,
       std::function<bool()> setup_fn,
       std::function<bool()> teardown_fn,
       std::string name);
  
  bool setup();
  bool teardown();
  Result run_test();
  std::string& name();
};

