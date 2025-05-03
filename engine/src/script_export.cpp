#include "lib_fixup.h"
#include "project.h"
#include "tmx_private.h"
#include "tmxreader.h"
#include <app.h>

ScriptExport::ScriptExport(std::string project_path, std::string dst): dst_directory(dst), project_path(project_path) {}

void ScriptExport::do_it() {

  Project *p = read_project(project_path.c_str());

  int i = 0;
  for(Script *scr: p->get_scripts()) {
    std::string path = dst_directory + "/" + scr->name;
    spit(path.c_str(), scr->script);
    i++;
  }

  printf("Exported %d scripts to %s\n", i, dst_directory.c_str());
}
