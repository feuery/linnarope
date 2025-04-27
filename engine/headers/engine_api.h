#pragma once

#include <ecl/ecl.h>

cl_object setup_scene(cl_object startup, cl_object update, cl_object teardown);
cl_object get_resource(cl_object _typename, cl_object resourcename);

void register_callbacks();
