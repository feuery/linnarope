#pragma once

/**
 * A homegrown automatic test assertion system.
 *
 * Written because trying to link googletest or
 * catch2 in github actions / archlinux:base-devel
 * was a disaster in the summer of 2025.
 */

#include <result.h>
#include <functional>

class Test {
private:
  Reporter reporter;
  
  std::function<bool(std::vector<Result>&)> test_fn;
  std::function<bool()> _setup, _teardown;
  std::string nme;

  std::vector<Result> results;
  
public:

  Test(std::function<bool(std::vector<Result>&)> test_fn,
       std::function<bool()> setup_fn,
       std::function<bool()> teardown_fn,
       std::string name);

  void report();
  
  bool setup();
  bool teardown();
  void run_test();
  std::string& name();
};

