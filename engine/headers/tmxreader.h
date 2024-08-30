#pragma #once

#include <SDL.h>
extern "C" {
  class Map;
  Map* read_map(const char *path);
  void render_map(Map *m, SDL_Renderer *r);
  void delete_map(Map*);
}
