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
