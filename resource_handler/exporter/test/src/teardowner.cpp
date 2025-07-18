#include <teardowner.h>

Teardowner::Teardowner(std::function<void()> l) : lambda(l) {};
Teardowner::~Teardowner() {
  lambda();
}
