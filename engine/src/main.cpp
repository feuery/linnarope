#include <cassert>
#include <cstdio>
#include <SDL.h>
#include "SDL_video.h"
#include "finrope.h"
#include <string>
#include "tmxreader.h"
#include <getopt.h>
#include <SDL_image.h>

struct cli_result {
  std::string map_path;
  std::string png_path;
  std::string sqlite_path;

  // returns true if there's anything to process and the app should thus close after returning here
  bool process();
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
    puts("cli_result::processing() \n");
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

  auto cliresult = handle_cli(argc, argv);

  if(cliresult.process()) {
    return 0;
  }

  SDL_Window *window = createWindow();

  assert(window);

  auto *renderer = createRenderer(window);
  assert(renderer);

  Map *map = read_map("/Users/feuer/Projects/finrope/maps/pikkustadi-töölön tulli.tmx");
  puts("Read the whole stadi!");

  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
  render_map(map, renderer);

  bool exit = false;
  SDL_Event eventData;
  while (!exit) {
    SDL_RenderClear(renderer);

    render_map(map, renderer);      
    SDL_RenderPresent(renderer);
       
    while (SDL_PollEvent(&eventData)) {
      switch (eventData.type) {
      case SDL_QUIT:
	exit = true;
	break;
      }
    }
  }

  delete_map(map);

  // Destroy the render, window and finalise SDL
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return 0;
}
