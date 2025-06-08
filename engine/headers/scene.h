#pragma once

#include <ecl/ecl.h>
#include <tmxreader.h>
#include <tmx_private.h>
#include <SDL.h>
#include <SDL_ttf.h>

#include <unordered_map>


/*
 * This class contains the current scene. It handles all the setup, teardown,
 * updates and map changing. It gets its resources from a Project* and has
 * an api for scripts to query for arbitrary resources.
 *
 * Also I think all the sdl drawing primitive calls (draw a line, draw a rect, etc...)
 * have to be routed throught this class.
 */


class Scene {
 private:
  Project *proj;
  
  cl_object current_startup, current_update, current_teardown;
  Map *current_map;

  std::unordered_map<SDL_Keycode, bool> keystate;

  SDL_Renderer *renderer;

  TTF_Font *font;
  SDL_Color current_color;
  
 public:
  SDL_PixelFormat* currentFormat();
  Scene(Project *p, SDL_Renderer *r);
  void changeMap(Map *m);
  
  void register_callbacks(cl_object startup, cl_object update, cl_object teardown);
  void update();
  int resource_to_handle(const char *typename_, const char *resourcename);
  Resource* handle_to_resource(int handle);

  // drawing primitives
  void line(int x1, int y1, int x2, int y2, int thickness);
  void setColor(Uint8 r, Uint8 g, Uint8 b);

  // drawing text
  void loadFont(const char* fontPath);
  void drawText(std::string &txt, int x, int y);

  // kbd   
  bool is_keydown(std::string& keystr);
  void setKeydown(SDL_Keycode keystr);
  void setKeyup(SDL_Keycode keystr);
};
