#pragma once

#include <SDL.h>
#include <vector>
#include "simulated_drawing.h"

class Map;
// std::vector<Map*> read_maps(const char *initial_tmx_path, const char *sqlite_path);
Map* read_map(const char *path, const char* sqlite_path = nullptr);

void render_map(Map *m, SDL_Renderer *r);
void delete_map(Map*);
SDL_Surface* map_surface(Map* m);

int map_w(Map *m);
int map_h(Map *m);

struct xy { int x; int y; };
void generate_drawing_context(Map *m, drawing_state *ctx, SDL_Renderer *r, std::vector<Map*>* visited_maps = nullptr, int depth = 1, xy parent_map_location = {0,0}, xy parent_warpzone_location = {0,0}, xy dst_warpzone_location = {0,0});
