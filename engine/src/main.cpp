#include "SDL_render.h"
#include <ecl/ecl.h>

#include <cassert>
#include <cstdio>
#include <SDL.h>
#include <finrope.h>
#include <tmxreader.h>
#include <getopt.h>
#include <SDL_image.h>
#include <sqlite3.h>
#include <swank.h>
#include <engine_api.h>
#include <scene.h>
#include <string>
#include <project.h>
#include <app.h>

Scene *current_scene = nullptr;
SDL_Renderer *current_renderer = nullptr;


void freeApp(App *app) { delete app; }

App* getApp (int argc, char **argv) {
  int c;
  static option opts[] = {
    // used when transforming a map into a png 
    {"map-file", required_argument, nullptr, 'm'},
    {"png-output-file", required_argument, nullptr, 'p'},
    // used when transforming a whole shitload of maps into a png 
    {"whole-map", required_argument, nullptr, 'w'},
    {"game", required_argument, nullptr, 'g'},
    { nullptr, 0, nullptr, 0}};

  std::string map = "",
    png = "",
    wholemaps_sourcedb_path = "",
    game = "";

  App *app = nullptr;
  

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
      wholemaps_sourcedb_path = std::string(optarg);
      break;
    case 'g':
      printf("Reading game from sqlite db %s\n", optarg);
      game = std::string(optarg);
      break;
    }
  }

  if (game != "") {
    app = new Game(game);
  }
  else if (wholemaps_sourcedb_path != "" && png != "") {
    app = new WholeMapRenderer(wholemaps_sourcedb_path, png);
  }
  else if (map != "" && png != "") {
    app = new SingleMapRenderer(map, png);
  }

  assert(app);

  return app;
}

int main (int argc, char **argv) {
  cl_boot(argc, argv);
  atexit(cl_shutdown);
  start_swank();

  register_callbacks();

  App* app = getApp(argc, argv);
  app->do_it();
  freeApp(app);

  return 0;
}
