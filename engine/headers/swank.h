#pragma once
#include <ecl/ecl.h>

void start_swank();

#define DEFUN(name,fun,args) \
    ecl_def_c_function(c_string_to_object(name), \
                      (cl_objectfn_fixed)fun, \
                      args)

cl_object ecl_call(const char *call);
cl_object lol();
