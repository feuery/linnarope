#include <catch2/catch_test_macros.hpp>
#include <cstdio>
#include <functional>

class Teardowner {
  std::function<void()> lambda;

public:

  Teardowner(std::function<void()> l);

  ~Teardowner();
};

Teardowner::Teardowner(std::function<void()> l) : lambda(l) {};
Teardowner::~Teardowner() {
  lambda();
}

TEST_CASE("Toimiiks?") {
  std::vector<int> vec;
  vec.push_back(55);

  Teardowner t([]() { puts("Running teardown"); });
  
  SECTION("sec1") {
    REQUIRE (255 == 255);

    vec.push_back(23);
    vec.push_back(24);
    
    puts("Vec contains:");
    for(auto a: vec) printf("%d, ", a);
    puts("");
  }

  SECTION("sec2") {

        vec.push_back(25);
    vec.push_back(26);
    
    puts("Vec contains:");
    for(auto a: vec) printf("%d, ", a);
    puts("");

    REQUIRE(23 == 24);

    puts("päästäänkö tänne?");
  }
}
