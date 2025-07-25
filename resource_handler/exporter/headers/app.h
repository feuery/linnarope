#pragma once

#include <string>

class App {
public: virtual void do_it (std::string &psql_connstring, std::string sqlite_path) = 0;

  virtual ~App();
};

class Importer: public App {
public: virtual void do_it (std::string &psql_connstring, std::string sqlite_path) override;
};

class Exporter: public App {
public: virtual void do_it (std::string &psql_connstring, std::string sqlite_path) override;
};


#define __user "linnarope"
#define __password "linnarope"
#define __db "linnarope"
#define __db_test "linnarope_test"

#define connection_string()                                                    \
  (std::string("postgresql://") + __user + ":" + __password + "@localhost:5432/" + __db)

// okay this is a ridiculous hack, where we connect to localhost:5432 on mac and db:5432 on linux, but docker networking is kind of odd in gha.
// for reasons I don't understand, localhost:5432 port binding doesn't work there AND I can't get hostname `db` to resolve on mac.
//
// so... sorry for anyone that might try to run these tests on linux in a not gha environment?

#ifdef __APPLE__
#define test_connection_string() \
  (std::string("postgresql://") + __user + ":" + __password + "@localhost:5432/" + __db_test)
#else
#define test_connection_string() \
  (std::string("postgresql://") + __user + ":" + __password + "@db:5432/" + __db_test)
#endif 
