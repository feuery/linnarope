#include <ecl/ecl.h>

#include <cassert>
#include <cstdio>
#include <SDL.h>
#include "SDL_surface.h"
#include "SDL_video.h"
#include "finrope.h"
#include <string>
#include "tmxreader.h"
#include <getopt.h>
#include <SDL_image.h>
#include <sqlite3.h>
#include "swank.h"

struct cli_result {
  std::string map_path;
  std::string png_path;
  std::string sqlite_path;

  std::string initial_tmx_path = "";

  // returns true if there's anything to process and the app should thus close after returning here
  bool process();
  void get_initial_tmx_path(const char *sqlite_path);
};

SDL_Window* createWindow(bool hidden = false) {
  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
    SDL_Log("SDL_Init Error: %s\n", SDL_GetError());
    return nullptr;
  }

  SDL_Window* window = SDL_CreateWindow("Hello World SDL", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, WINDOW_WIDTH, WINDOW_HEIGHT, hidden? SDL_WINDOW_HIDDEN: 
					SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE);
  if (window == nullptr) {
    SDL_Log("SDL_CreateWindow Error: %s\n", SDL_GetError());
    return nullptr;
  }

  return window;
}

SDL_Renderer* createRenderer(SDL_Window *window) {
  SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if (renderer == nullptr) {
    SDL_Log("SDL_CreateRenderer Error: %s\n", SDL_GetError());
    return nullptr;
  }

  return renderer;
}

// returning std::string seems to be fucking impossible in c++ (due to
// std::string missing a copy constructor?) so this returns its value in a really
// dumb way through the lexical this-> scope
void cli_result::get_initial_tmx_path(const char *sqlite_path) {
  sqlite3 *db;
  sqlite3_stmt *stmt;
  sqlite3_open(sqlite_path, &db);

  sqlite3_prepare(db, "SELECT tmx_path FROM Map WHERE ID IN (SELECT src_map FROM warp_connection) LIMIT 1", -1, &stmt, nullptr);

  if(sqlite3_step(stmt) == SQLITE_ROW) {
    auto txt = sqlite3_column_text(stmt, 0);
    assert(txt);
    this->initial_tmx_path = std::string (reinterpret_cast<const char*>(txt));
  }
  else puts("Didn't find a suitable map");
  
  sqlite3_close(db);
}

bool cli_result::process() {
  assert(map_path == "" || sqlite_path == "");
  
  if(map_path != "" && png_path != "") {

    SDL_Window *w = createWindow(true);
    SDL_Renderer *r = createRenderer(w);
    
    Map *m = read_map(map_path.c_str());
    render_map(m, r);

    IMG_SavePNG(map_surface(m), png_path.c_str());

    printf("Saved %s as %s\n", map_path.c_str(), png_path.c_str());
    
    // tallennetaan karttaa png:ksi
    return true;
  }
  else if (sqlite_path != "" && png_path != "") {
    get_initial_tmx_path(sqlite_path.c_str());

    assert(initial_tmx_path != "");
    
    puts("cli_result::processing() \n");
    printf("Making a huge map of %s and %s\n", sqlite_path.c_str(), initial_tmx_path.c_str());

    SDL_Window *w = createWindow(true);
    SDL_Renderer *r = createRenderer(w);

    Map *first = read_map(initial_tmx_path.c_str(), sqlite_path.c_str());
    
    drawing_state *ctx = start_drawing();

    generate_drawing_context(first, ctx, r);

    SDL_Surface *final_map = stop_drawing(ctx);

    IMG_SavePNG(final_map, png_path.c_str());

    printf("Saved the whole map (%d x %d) to %s\n", final_map->w, final_map->h, png_path.c_str());
    SDL_FreeSurface(final_map);
    
    return true;
  }

  return false;
}

cli_result handle_cli(int argc, char **argv) {
  int c;
  static option opts[] = {
    {"map-file", required_argument, nullptr, 'm'},
    {"png-output-file", required_argument, nullptr, 'p'},
    {"whole-map", required_argument, nullptr, 'w'},
    { nullptr, 0, nullptr, 0}};

  std::string map = "",
    png = "",
    sqlite_path = "";
  

  while ((c = getopt_long(argc, argv, "m:p:w:", opts, nullptr)) != -1) {
    switch(c) {
    case 'm':
      printf("map-file found %s\n", optarg);
      map = std::string(optarg);
      break;
    case 'p':
      printf("png-output-file discovered: %s\n", optarg);
      png = std::string(optarg);
      break;
    case 'w':
      printf("Reading maps from an sqlite db %s\n", optarg);
      sqlite_path = std::string(optarg);
      break;
    }
  }

  printf("Read the sqlite path %s\n", sqlite_path.c_str());

  return { map, png, sqlite_path};
}

int main (int argc, char **argv) {

  cl_boot(argc, argv);
  atexit(cl_shutdown);
  start_swank();
  
  auto cliresult = handle_cli(argc, argv);

  if(cliresult.process()) {
    return 0;
  }

  SDL_Window *window = createWindow();

  assert(window);

  auto *renderer = createRenderer(window);
  assert(renderer); 
  // Map *map = read_map("/Users/feuer/Projects/finrope/maps/pikkustadi-toolon tulli.tmx",
  // 		      "/Users/feuer/Projects/finrope/resource_handler/resources.db");
  puts("Read the whole stadi!");

  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
  // render_map(map, renderer);

  bool exit = false;
  SDL_Event eventData;
  while (!exit) {
    SDL_RenderClear(renderer);

    // render_map(map, renderer);      
    SDL_RenderPresent(renderer);
       
    while (SDL_PollEvent(&eventData)) {
      switch (eventData.type) {
      case SDL_QUIT:
	exit = true;
	break;
      }
    }
  }

  // delete_map(map);

  // Destroy the render, window and finalise SDL
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return 0;
}
