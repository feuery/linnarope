#include <stdexcept>
#include <tmxreader.h>
#include <engine_api.h>
#include <finrope.h>
#include <cstdio>
#include <swank.h>


#include <scene.h>

void register_callbacks() {
  DEFUN("setup-scene", setup_scene, 3);
  DEFUN("get-resource", get_resource, 2);
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
  int handle = current_scene->get_resource(typename_.c_str(), resourcename_.c_str());

  return ecl_make_int(handle);
  } catch (std::out_of_range _) {
    return ECL_NIL;
  }
}
