#pragma once

#include <ecl/ecl.h>

cl_object setup_scene(cl_object startup, cl_object update, cl_object teardown);
cl_object get_resource(cl_object _typename, cl_object resourcename);
cl_object change_map(cl_object map_handle);
cl_object is_keydown(cl_object keystr);
cl_object timer();
cl_object draw_line(cl_object x1, cl_object y1, cl_object x2, cl_object y2,
                    cl_object thickness);
cl_object set_color(cl_object r, cl_object g, cl_object b);
cl_object draw_text(cl_object txt, cl_object x, cl_object y);

void register_callbacks();
