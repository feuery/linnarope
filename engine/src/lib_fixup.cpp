#include <cstdio>
#include <stdio.h>
#include <cassert>
#include <lib_fixup.h>

std::string get_file_contents(const char *filename) {

  FILE *fp = fopen(filename, "rb");
  if (fp)
  {
    std::string contents;
    fseek(fp, 0, SEEK_END);
    contents.resize(ftell(fp));
    rewind(fp);
    fread(&contents[0], 1, contents.size(), fp);
    fclose(fp);
    return(contents);
  }
  throw(errno);
}

void spit(const char *filename, std::string &contents) {
  FILE *f = fopen(filename, "w");

  assert(f);

  fwrite(contents.c_str(), sizeof(char), contents.size(), f);
  fclose(f);
}
