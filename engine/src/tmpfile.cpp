#include <cstdlib>
#include <unistd.h>
#include <cstring>
#include <tmpfile.h>
#include <cassert>

std::string get_next_tmpfilename() {
  // // static const char *file_template = "/tmp/linnaropeXXXXXXXXXXXXXXXXXX";
  // but initialized in a mutable way that doesn't crash this application
  char filename[33] = {'/', 't', 'm', 'p', '/', 'l', 'i', 'n', 'n', 'a', 'r', 'o', 'p', 'e', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', '\0'};

  auto descriptor = mkstemp(filename);
  printf("Generated filename %s, d %d\n", filename, descriptor);
  assert(descriptor != -1);

  if (descriptor < 0) throw "mkstemp failed";

  std::string fname(filename);

  return fname;
}
