#pragma once
#include <ecl/ecl.h>

void start_swank();

#define DEFUN(name,fun,args) \
    ecl_def_c_function(c_string_to_object(name), \
                      (cl_objectfn_fixed)fun, \
                      args)

cl_object ecl_call(const char *call);
cl_object lol();

// copypasted from my ancient repo
// https://github.com/feuery/qmapper/blob/0bb62e54164871356342b1e01d1feb06762c799a/src/cpp/guile_fn.cpp#L46
// no clue where this function appeared there though
std::string ecl_string_to_string(cl_object);
