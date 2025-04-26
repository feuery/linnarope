#pragma once

#include <ecl/ecl.h>

cl_object setup_scene(cl_object startup, cl_object update,
                      cl_object teardown);

void register_callbacks();
