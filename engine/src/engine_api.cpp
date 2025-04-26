#include <tmxreader.h>
#include <engine_api.h>
#include <finrope.h>
#include <cstdio>
#include <swank.h>


#include <scene.h>

void register_callbacks() {
  DEFUN("setup-scene", setup_scene, 3);
  DEFUN("lol", lol, 0);
}

cl_object setup_scene(cl_object startup, cl_object update, cl_object teardown) {
  if(!current_scene) {
    puts("Current scene is not set up :(\n");
    return ECL_NIL;
  }
  current_scene->register_callbacks(startup, update, teardown);
  
  puts("Registered a new scene");
  return ecl_call("\"Registered a new scene\"");
}
