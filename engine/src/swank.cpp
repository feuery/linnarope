#include <ecl/ecl.h>
#include <swank.h>
#include <string>

std::string swank_location = "/Users/feuer/quicklisp/dists/quicklisp/software/slime-v2.30/swank.asd",
  ql_location = "/Users/feuer/quicklisp.lisp";

cl_object lol() {
  puts("lol :D\n");
  return ecl_make_integer(1234);
}

void start_swank() {
  const cl_env_ptr l_env = ecl_process_env();
  CL_CATCH_ALL_BEGIN(l_env) {
    CL_UNWIND_PROTECT_BEGIN(l_env) {
      cl_require(1, ecl_make_keyword("asdf"));

      std::string asdf_load_call = "(load \"" + ql_location + "\")";
      ecl_call("(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\" \
                                       (user-homedir-pathname)))) \
  (when (probe-file quicklisp-init) \
    (load quicklisp-init)))");
      puts("Do we have quicklisp? \n");
      
      // ecl_call(asdf_load_call.c_str());

      // ecl_call("(asdf:load-system :swank)");
      // ecl_call("(asdf:load-system :swank-indentation)");
      // ecl_call("(swank-loader:init :load-contribs t)");
      // ecl_call("(swank:create-server :dont-close t)");

      ecl_call("(ql:quickload :swank)");
      ecl_call("(swank:create-server :dont-close t)");
    }
    CL_UNWIND_PROTECT_EXIT {}
    CL_UNWIND_PROTECT_END;
  }
  CL_CATCH_ALL_END;
}


cl_object ecl_call(const char *call){
  return cl_safe_eval(c_string_to_object(call), Cnil, Cnil);
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
