#include <stdio.h>
#include <string>
#include <vector>
#include <app.h>

std::vector<std::string> transform_argv(int argc, char **argv) {
  std::vector<std::string> acc;
  for(int i=0; i < argc; i++) {
    acc.push_back(std::string(argv[i]));
  }

  return acc;
}

int main(int argc, char **argv) {

  App *app;

  std::string dst_sqlite_path = "";
  auto args = transform_argv(argc, argv);
  
  switch(argc) {
  case 2:
    if(args.at(1) != "-test") {
      dst_sqlite_path = argv[1];
      app = new Exporter;
    }
    else {
      app = new AutoTests(HUMAN);
    }
    break;
  case 3:
    if(args.at(1) != "-test") {
      dst_sqlite_path = argv[2];
      app = new Importer;
    }
    else {
      puts("Running tests with json reporter");
      app = new AutoTests(JSON);
    }

    break;
  default:
    printf("Usage: exporter projectfilename\nOr: exporter import projectfilename");
    return -1;
  }

  app->do_it(dst_sqlite_path);

  delete app;
  return 0;
}
