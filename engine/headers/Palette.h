#pragma once

#include <cstdint>
#include <tmx_private.h>
#include <vector>

struct RGBTuple {
public:
  uint8_t r, g, b;
};

class Palette: public Resource {
private:
  std::vector<RGBTuple> rgbs;
  std::string name;
public:

  Palette(std::string name, std::string color_json);
  Palette();

  RGBTuple getColor(int index);
  
  const char* get_typename() override;  
};
