#include <cassert>
#include <cstdio>
#include <SDL.h>
#include "finrope.h"
#include "tmxreader.h"
#include <getopt.h>

enum continue_state { CONTINUE, EXIT };

continue_state handle_cli(int argc, char **argv) {
  int c;
  static option opts[] = {
    {"map-file", required_argument, nullptr, 'm'},
    {"png-output-file", required_argument, nullptr, 'p'},
    { nullptr, 0, nullptr, 0}};

  while ((c = getopt_long(argc, argv, "m:p:", opts, nullptr)) != -1) {
    switch(c) {
    case 'm':
      printf("map-file found %s\n", optarg);
      break;
    case 'p':
      printf("png-output-file discovered: %s\n", optarg);
      break;
    }
  }

  return EXIT;
}

int main (int argc, char **argv) {

  if (handle_cli(argc, argv) == EXIT) {
    return 0;
  }

  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
    SDL_Log("SDL_Init Error: %s\n", SDL_GetError());
    return -1;
  }

  SDL_Window* window = SDL_CreateWindow("Hello World SDL", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE);
  if (window == nullptr) {
    SDL_Log("SDL_CreateWindow Error: %s\n", SDL_GetError());
    return -1;
  }

  // Render creation
  SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if (renderer == nullptr) {
    SDL_Log("SDL_CreateRenderer Error: %s\n", SDL_GetError());
    return -1;
  }

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
