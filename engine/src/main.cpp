#include <cstdio>
#include <SDL.h>
#include "finrope.h"
#include "tmxreader.h"

int main (int argc, char **argv) {

  printf("random number: %d\n", random_number());
  return 0;

  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
    SDL_Log("SDL_Init Error: %s\n", SDL_GetError());
    return -1;
  }

   SDL_Window* window = SDL_CreateWindow("Hello World SDL", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN);
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

    SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);

    bool exit = false;
    SDL_Event eventData;
    while (!exit) {
      SDL_RenderClear(renderer);
      SDL_RenderPresent(renderer);
       
      while (SDL_PollEvent(&eventData)) {
	switch (eventData.type) {
	case SDL_QUIT:
	  exit = true;
	  break;
	}
      }
    }

    // Destroy the render, window and finalise SDL
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return 0;
}
