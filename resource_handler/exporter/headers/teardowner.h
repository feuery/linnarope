#pragma once

#include <functional>

class Teardowner {
  std::function<void()> lambda;

public:

  Teardowner(std::function<void()> l);

  ~Teardowner();
};
