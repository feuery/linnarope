#pragma once

#include <string>
#include <vector>
#include <test.h>

class App {
protected:
  std::string postgres_connstring();
  
public:
  bool isTest;
  virtual void do_it (std::string sqlite_path) = 0;
  App();
  virtual ~App();
};

class Importer: public App {
public: virtual void do_it (std::string sqlite_path) override;
};

class Exporter: public App {
public: virtual void do_it (std::string sqlite_path) override;
};

class AutoTests: public App {
private:
  std::vector<Test> get_tests();
  
public:
  virtual void do_it (std::string sqlite_path) override;
  AutoTests();
  void RunAndReportTests();
};


const std::string __user = "linnarope";
const std::string __password = "linnarope";
const std::string __db = "linnarope";
const std::string __db_test = "linnarope_test";

// okay this is a ridiculous hack, where we connect to localhost:5432 on mac and db:5432 on linux, but docker networking is kind of odd in gha.
// for reasons I don't understand, localhost:5432 port binding doesn't work there AND I can't get hostname `db` to resolve on mac.
//
// so... sorry for anyone that might try to run these tests on linux in a not gha environment?

#ifdef __APPLE__

const std::string test_connection_string = (std::string("postgresql://") + __user + ":" + __password + "@localhost:5432/" + __db_test);
#else
  const std::string test_connection_string = (std::string("postgresql://") + __user + ":" + __password + "@db:5432/" + __db_test);
#endif 


const std::string connection_string = (std::string("postgresql://") + __user + ":" + __password + "@localhost:5432/" + __db);
