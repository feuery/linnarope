#pragma once

#include <ecl/ecl.h>

#include <tmxreader.h>


/*
 * This class contains the current scene. It handles all the setup, teardown,
 * updates and map changing. It gets its resources from a Project* and has
 * an api for scripts to query for arbitrary resources.
 */


class Scene {
 private:
  // Project *proj;
  
  cl_object current_startup, current_update, current_teardown;
  
 public:
  Scene(// Project *p
	);
  void changeMap(Map *m);
  
  void register_callbacks(cl_object startup, cl_object update, cl_object teardown);
  void update();
};
