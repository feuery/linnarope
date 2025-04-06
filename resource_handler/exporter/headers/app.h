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
