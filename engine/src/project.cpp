#include <tmx_private.h>
#include <project.h>
#include <cassert>

void Project::insertScript(int id, const char *name, Script scr) {
  scripts[name] = scr;
  id_to_scripts[id] = &scripts[name];
}

Script &Project::getScript(const char *nme) {
  assert(!scripts.empty());
  return scripts.at(nme);
}
Script &Project::getScript(int id) {
  assert(!id_to_scripts.empty());
  Script *scr = id_to_scripts.at(id);
  return *scr;
}


bool Project::hasScript(int id) {
  return id_to_scripts.contains(id);
}
