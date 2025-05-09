
#include "Palette.h"
#include "SDL_pixels.h"
#include "SDL_rect.h"
#include "SDL_render.h"
#include "SDL_surface.h"
#include "finrope.h"
#include <cassert>
#include <cstddef>
#include <lisp_sprite.h>

const char *Lisp_sprite::get_typename() { return "Lisp sprite"; }
const char* Palette::get_typename() { return "Palette"; }

Lisp_sprite::Lisp_sprite() {}
Lisp_sprite::Lisp_sprite(int id, std::string nme, int w, int h, Palette& p): prerendered(nullptr),
									     prerendered_txt(nullptr),
									     id(id),
									     w(w),
									     h(h),
									     palette(&p),
									     name(nme) {}
Lisp_sprite::Lisp_sprite(Lisp_sprite& spr): prerendered(spr.prerendered),
					    prerendered_txt(spr.prerendered_txt),
					    id(spr.id),
					    w(spr.w),
					    h(spr.h),
					    palette(spr.palette),
					    name(spr.name)
{ }


void Lisp_sprite::render_to_screen(int x, int y) {
  if(!prerendered_txt) rerender();
  assert(prerendered_txt);
  assert(current_renderer);

  SDL_Rect loc = {x, y, prerendered->w, prerendered->h};
  SDL_RenderCopy(current_renderer, prerendered_txt, nullptr, &loc);
}

void Lisp_sprite::rerender() {
  assert(current_scene);
  assert(current_renderer);
  
  if(prerendered) {
    SDL_FreeSurface(prerendered);
    prerendered_txt = nullptr;
    prerendered = nullptr;
  }   

  prerendered = SDL_CreateRGBSurface(0,
				     w, h,
				     32,
				     0, 0, 0, 0);

  for(int x = 0; x < w; x++) {
    for(int y = 0; y < h; y++) {
      int color_index = pixels.at(x).at(y);
      RGBTuple tuple = this->palette->getColor(color_index);
      auto color = SDL_MapRGB(current_scene->currentFormat(), tuple.r, tuple.g, tuple.b);

      SDL_Rect loc = {x, y, 1, 1};
      SDL_FillRect(prerendered, &loc, color);      
    }
  }

  prerendered_txt = SDL_CreateTextureFromSurface(current_renderer, prerendered);
}
