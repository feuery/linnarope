#include <SDL_rect.h>
#include <SDL_render.h>
#include <finrope.h>
#include <cassert>

#include <sprite.h>

const char *Sprite::get_typename() { return "Sprite"; }
Sprite::Sprite(std::string nme, SDL_Surface *sprite): nme(nme), srfc(sprite), txt(nullptr) {
  assert(current_renderer);
  txt = SDL_CreateTextureFromSurface(current_renderer, srfc);
}
Sprite::Sprite(Sprite& spr) {
  this->nme = spr.nme;
  this->srfc = spr.srfc;
  this->txt = spr.txt;
}

Sprite::Sprite() {}

void Sprite::render_to_screen(int x, int y) {
  assert(current_renderer);
  assert(txt);
  SDL_Rect loc = {x, y, srfc->w, srfc->h};
  SDL_RenderCopy(current_renderer, txt, nullptr, &loc);
}
