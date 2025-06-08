#include "SDL_ttf.h"
#include <app.h>
#include <finrope.h>
#include <project.h>
#include <ecl/ecl.h>
#include <swank.h>
#include <engine_api.h>

App::~App() { }

SDL_Renderer* App::createRenderer(SDL_Window *window) {
  SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if (renderer == nullptr) {
    SDL_Log("SDL_CreateRenderer Error: %s\n", SDL_GetError());
    return nullptr;
  }

  return renderer;
}

SDL_Window* App::createWindow(bool hidden) {
  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
    SDL_Log("SDL_Init Error: %s\n", SDL_GetError());
    return nullptr;
  }

  if(TTF_Init() != 0) {
    SDL_Log("TTF_Init Error: %s\n", TTF_GetError());
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

Game::Game(int argc, char **argv, std::string sqlite_path): sqlite_path(sqlite_path), argc(argc), argv(argv) { }

void Game::do_it() {
  cl_boot(argc, argv);
  atexit(cl_shutdown);
  start_swank();

  register_callbacks();

  
  SDL_Window *window = createWindow();

  assert(window);

  auto *renderer = createRenderer(window);
  assert(renderer);
  current_renderer = renderer;

  Project* proj = read_project(sqlite_path.c_str());

  auto &p = *proj->maps.begin();
  Map *map = &p.second;

  puts("Loaded maps: ");
  for(auto& pair: proj->maps){
    printf("\"%s\", ", pair.first.c_str());
  }
  puts("\n");

  map_x(map, 0);
  map_y(map, 0);

  // SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
  render_map(map, renderer);

  assert_map_makes_sense(map);

  bool exit = false;
  SDL_Event eventData;

  Scene scn(proj, renderer);
  current_scene = &scn;
  scn.changeMap(map);
  

  #ifdef __APPLE__
  // TODO this is some sort of default arial that comes with mac.
  // Replace it with a ttf that I can package with this app. 
  scn.loadFont("/System/Library/Fonts/Supplemental/Arial Unicode.ttf");
  #else
  #error Fonts are broken
  #endif 
  
  
  while (!exit) {
    SDL_RenderClear(renderer);

    // render_map(map, renderer);      
    
       
    while (SDL_PollEvent(&eventData)) {
      switch (eventData.type) {
      case SDL_QUIT:
	exit = true;
	break;

      case SDL_KEYDOWN:
	scn.setKeydown(eventData.key.keysym.sym);
	break;

      case SDL_KEYUP:
	scn.setKeyup(eventData.key.keysym.sym);
	break;
      }
    }

    scn.update();
    SDL_RenderPresent(renderer);
  }

  // delete_map(map);

  // Destroy the render, window and finalise SDL
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit(); 
}
