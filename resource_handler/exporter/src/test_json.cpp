#include <test_json.h>

using json = nlohmann::json;

void to_json(nlohmann::json &j, Test *test) {
  
  j = json {{ "element", "testsuite"},
	    { "name", test->name()} ,
	    { "results", test->results}};
}


//  <testcase name="testCase5" classname="Tests.Authentication.Login"
//  time="0.781" />
// or
//  <testcase name="testCase9" classname="Tests.Authentication" time="0.982">
//             <failure message="Assertion error message" type="AssertionError">
//                 <!-- Call stack printed here -->
//             </failure>
//         </testcase>

void to_json(nlohmann::json &j, const Result &result) {
  to_json(j, &result);
}

void to_json(nlohmann::json &j, const Result *result) {
  j = json { {"element", "testcase"},
	     {"name", result->code},
	     {"classname", std::string("feuertest") + std::to_string(rand() % 1000)},
	     {"time", 0.98765}};  
}
