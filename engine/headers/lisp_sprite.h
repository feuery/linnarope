#pragma once

#include "SDL_render.h"
#include "SDL_surface.h"
#include <SDL.h>
#include <string>
#include <tmx_private.h>
#include <unordered_map>

#include <Palette.h>

typedef int pixel;

class Lisp_sprite: public Resource {
private:
  SDL_Surface *prerendered;
  SDL_Texture *prerendered_txt;
  
public:

  int id, w, h;
  Palette *palette;
  std::string name;

  std::unordered_map<int, std::unordered_map<int, pixel>> pixels; // where pixel is an index to palette->rgbs

  void rerender();
  
  const char* get_typename() override;
  void render_to_screen(int x, int y) override;

  Lisp_sprite(int id, std::string nme, int w, int h, Palette& p);
  Lisp_sprite(Lisp_sprite&);
  Lisp_sprite();
};
