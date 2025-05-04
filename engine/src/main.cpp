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
    {"export-script-to", required_argument, nullptr, 'e'},
    {"import-scripts-from", required_argument, nullptr, 'i'},
    { nullptr, 0, nullptr, 0}};

  std::string map = "",
    png = "",
    wholemaps_sourcedb_path = "",
    game = "",
    export_dst_dir = "",
    import_src_dir = "";

  App *app = nullptr;
  
  while ((c = getopt_long(argc, argv, "m:p:w:g:e:", opts, nullptr)) != -1) {
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
    case 'e':
      printf("Exporting scripts to %s\n", optarg);
      export_dst_dir = std::string(optarg);
      break;
    case 'i':
      printf("Importing scripts from %s\n", optarg);
      import_src_dir = std::string(optarg);
      break;
    }
  }

  if (game != "" && export_dst_dir == "" && import_src_dir != "") {
    app = new ScriptImport(game, import_src_dir);
  }
  else if (game != "" && export_dst_dir != "" && import_src_dir == "") {
    app = new ScriptExport(game, export_dst_dir);
  }
  else if (game != "" && export_dst_dir == "") {
    printf("Creating game app \"%s\"\n", game.c_str());
    app = new Game(game);
  }
  else if (wholemaps_sourcedb_path != "" && png != "") {
    app = new WholeMapRenderer(wholemaps_sourcedb_path, png);
  }
  else if (map != "" && png != "") {
    app = new SingleMapRenderer(map, png);
  }
  else throw "Unknown app state";

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
