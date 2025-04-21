#pragma once

#include <SDL.h>
#include <vector>
#include "simulated_drawing.h"

extern "C" {
  class Project;
  class Map;
  class Script;
  
  Project* read_project(const char *path);
  Map* getMaps(Project *proj, int &count_of_maps);


  void render_map(Map *m, SDL_Renderer *r);
  void delete_map(Map*);
  SDL_Surface* map_surface(Map* m);

  int map_w(Map *m);
  int map_h(Map *m);

  void map_x(Map*, int);
  void map_y(Map*, int);

  struct xy { int x; int y; };
  void generate_drawing_context(Project *proj, Map *m, drawing_state *ctx, SDL_Renderer *r, std::vector<Map*>* visited_maps = nullptr, int depth = 1, xy parent_map_location = {0,0}, xy parent_warpzone_location = {0,0}, xy dst_warpzone_location = {0,0});
}
