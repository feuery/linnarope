#pragma once

#include <ecl/ecl.h>
#include <tmxreader.h>
#include <tmx_private.h>
#include <SDL.h>

#include <unordered_map>


/*
 * This class contains the current scene. It handles all the setup, teardown,
 * updates and map changing. It gets its resources from a Project* and has
 * an api for scripts to query for arbitrary resources.
 */


class Scene {
 private:
  Project *proj;
  
  cl_object current_startup, current_update, current_teardown;
  Map *current_map;

  std::unordered_map<SDL_Keycode, bool> keystate;
  
 public:
  SDL_PixelFormat* currentFormat();
  Scene(Project *p);
  void changeMap(Map *m);
  
  void register_callbacks(cl_object startup, cl_object update, cl_object teardown);
  void update();
  int resource_to_handle(const char *typename_, const char *resourcename);
  Resource* handle_to_resource(int handle);


  bool is_keydown(std::string& keystr);
  void setKeydown(SDL_Keycode keystr);
  void setKeyup(SDL_Keycode keystr);
};
