#include <stdio.h>
#include <string>
#include <app.h>

int main(int argc, char **argv) {

  App *app;

  std::string connString = connection_string(),
    dst_sqlite_path = "";
  
  switch(argc) {
  case 2:
    dst_sqlite_path = argv[1];
    app = new Exporter;
    break;
  case 3:
    dst_sqlite_path = argv[2];
    app = new Importer;
    break;
  default:
    printf("Usage: exporter projectfilename\nOr: exporter import projectfilename");
    return -1;
  }

  app->do_it(connString, dst_sqlite_path);

  delete app;
  return 0;
}
