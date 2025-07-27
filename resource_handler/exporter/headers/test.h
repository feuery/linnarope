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
  std::function<bool(std::vector<Result>&)> test_fn;
  std::function<bool()> _setup, _teardown;
  std::string nme;
  
public:
  // don't touch unless you're test_json.h's to_json()
  std::vector<Result> results;

  

  Test(std::function<bool(std::vector<Result>&)> test_fn,
       std::function<bool()> setup_fn,
       std::function<bool()> teardown_fn,
       std::string name);

  void report(Reporter reporter);
  
  bool setup();
  bool teardown();
  void run_test();
  std::string& name();
};

