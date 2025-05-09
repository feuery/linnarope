#include <Palette.h>
#include <cstdint>
#include <nlohmann/json.hpp>
#include <string>

std::string drop_hash(const std::string& orig) {
  if(orig.at(0) != '#') return orig;

  std::string toret;

  for(int i = 1; i < orig.size(); i++) {
    toret += orig[i];
  }

  return toret;
}

RGBTuple fromHex(std::string hex) {
  unsigned int nro = std::stoi(hex, nullptr, 16);

  uint8_t r = (nro >> 16),
    g = (nro >> 8) & 0xff,
    b = (nro & 0xff);

  printf("%s is a color of %d, %d, %d\n", hex.c_str(), r, g, b);

  return {r, g, b};
}

Palette::Palette(std::string name, std::string color_json) : name(name)
{
  auto array = nlohmann::json::parse(color_json);

  for(auto &color: array) {
    // colors are "#123456" hexes
    std::string actual_color = drop_hash(color);
    
    this->rgbs.push_back(fromHex(actual_color));
  }
}

Palette::Palette(){}

RGBTuple Palette::getColor(int index) {
  return rgbs.at(index);
}
