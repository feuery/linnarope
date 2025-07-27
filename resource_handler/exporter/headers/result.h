#pragma once

#include <string>

enum TestResult: bool { SUCCESS = true, FAILURE = false};

extern const std::string appname; // main.o is supposed to populate this...
std::string json_output_file();

enum Reporter { HUMAN, JSON /*json could be transformed into junit xml with a compiler that's yet to be written */ };
enum AssertionMacro { ensure, ensure_throws, ensure_nothrow};

#include <string>

class Result {
public:  
  TestResult result;
  std::string code;
  void report(Reporter reporter);
  AssertionMacro macro;
};


#define ENSURE(body)							\
  if((body)) {								\
    results.push_back(Result { SUCCESS, #body, ensure});		\
  } else { results.push_back(Result { FAILURE, #body, ensure}); }

#define ENSURE_THROWS(body, ex_class)				\
  try {								\
    body;							\
    results.push_back(Result { FAILURE, #body, ensure_throws}); \
  } catch(ex_class ex) {  results.push_back(Result { SUCCESS, #body, ensure_throws}); }

#define ENSURE_NOTHROW(body)						\
  try {									\
    body;								\
    results.push_back(Result { SUCCESS, #body, ensure_nothrow});	\
  } catch (...) {							\
    results.push_back(Result { FAILURE, #body, ensure_nothrow});	\
      }
    
  
