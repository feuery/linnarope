#include "tmx_private.h"
#include <project.h>

void Project::insertScript(int id, const char *name, Script scr) {
  scripts[name] = scr;
  id_to_scripts[id] = &scripts[name];
}

Script &Project::getScript(const char *nme) { return scripts.at(nme); }
Script &Project::getScript(int id) {
  Script *scr = id_to_scripts.at(id);
  return *scr;
}

