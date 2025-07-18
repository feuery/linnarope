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

#define test_connection_string() \
  (std::string("postgresql://") + __user + ":" + __password + "@localhost:5432/" + __db_test)
