#include "ecl/ecl.h"
#include "tmx_private.h"
#include <stdexcept>
#include <tmxreader.h>
#include <engine_api.h>
#include <finrope.h>
#include <cstdio>
#include <swank.h>

#include <scene.h>

cl_object render(cl_object handle, cl_object x_, cl_object y_);

void register_callbacks() {
  DEFUN("setup-scene", setup_scene, 3);
  DEFUN("get-resource", get_resource, 2);
  DEFUN("render", render, 3);
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

cl_object get_resource(cl_object _typename, cl_object resourcename) {
  assert(current_scene);

  std::string typename_ = ecl_string_to_string(_typename),
    resourcename_ = ecl_string_to_string(resourcename);  

  try {
    int handle = current_scene->resource_to_handle(typename_.c_str(), resourcename_.c_str());

    return ecl_make_int(handle);
  } catch (std::out_of_range _) {
    return ECL_NIL;
  }
}

cl_object render(cl_object handle, cl_object x_, cl_object y_) {
  assert(current_scene);

  int handle_ = ecl_to_int(handle),
    x = ecl_to_int(x_),
    y = ecl_to_int(y_);
  Resource *resource = current_scene->handle_to_resource(handle_);  

  resource->render_to_screen(x, y);
  
  return ECL_NIL;
}
