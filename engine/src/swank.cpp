#include <lib_fixup.h>
#include <tmpfile.h>
#include <ecl/ecl.h>
#include <swank.h>
#include <string>

std::string swank_location = "/Users/feuer/quicklisp/dists/quicklisp/software/slime-v2.30/swank.asd",
  ql_location = "/Users/feuer/quicklisp.lisp";

void start_swank() {
  ecl_call("(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\" \
                                       (user-homedir-pathname)))) \
  (when (probe-file quicklisp-init) \
    (load quicklisp-init)))");
  
  puts("Do we have quicklisp? \n");
  ecl_call("(ql:quickload :swank)");
  ecl_call("(swank:create-server :dont-close t)");
}

cl_object actual_eval(const char *call) {
  return cl_safe_eval(c_string_to_object(call), Cnil, Cnil);
}

cl_object ecl_call(const char *call){

  
  std::string fname = get_next_tmpfilename();
  std::string ccall = call;
  spit(fname.c_str(), ccall);

  std::string load_call = "(load #P\"" + fname + "\")";

  printf("Loading %s (calling %s) \n", fname.c_str(), load_call.c_str());

  return actual_eval(load_call.c_str());
}

// copypasted from my ancient repo
// https://github.com/feuery/qmapper/blob/0bb62e54164871356342b1e01d1feb06762c799a/src/cpp/guile_fn.cpp#L46
// no clue where this function appeared there though
std::string ecl_string_to_string(cl_object echar) {
  switch (ecl_t_of(echar)) {
#ifdef ECL_UNICODE
  case t_string:
    if (!ecl_fits_in_base_string(echar)) {
      echar = cl_copy_seq(echar);
    } else {
      echar = si_copy_to_simple_base_string(echar);
    }
    break;
#endif
  case t_base_string:
    // OK
    break;
  default:
    // PRINT SOME ERROR
    return std::string(); // or raise an exception
  }

  std::string res("");
  int j = echar->base_string.dim; //get dimension   
  ecl_base_char* selv = echar->base_string.self; //get pointer   

  //do simple pointer addition
  for(int i=0;i<j;i++){
    res += (*(selv+i));
  }
  return res;
}
