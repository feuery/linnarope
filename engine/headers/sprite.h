#pragma once

#include <SDL.h>
#include <string>
#include <tmx_private.h>

class Sprite: public Resource {
  std::string nme;
  SDL_Surface *srfc;
  SDL_Texture *txt;
public:

  Sprite(std::string nme, SDL_Surface *sprite);
  Sprite(Sprite&);
  Sprite();
  
  const char* get_typename() override;
  void render_to_screen(int x, int y) override;
};
