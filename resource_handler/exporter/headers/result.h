#pragma once

enum TestResult: bool { SUCCESS = true, FAILURE = false}; 

#include <string>

class Result {
public:
  TestResult result;
  std::string code;
};


#define ENSURE(body)						\
  if((body)) {							\
    /*printf("%s succeeded\n", #body);*/			\
  } else { printf ("FAIL: \t%s\n", #body); return false; }

#define ENSURE_THROWS(body, ex_class)				\
  try {								\
    body;							\
    printf("FAIL: %s didn't throw %s\n", #body, #ex_class);	\
    return false;						\
  } catch(ex_class ex) {  /* success*/ }

#define ENSURE_NOTHROW(body)	       \
  try {				       \
    body;			       \
  /* success */			       \
  } catch (...) {		       \
    printf("FAIL: %s threw\n", #body); \
    return false;		       \
  }
    
  
