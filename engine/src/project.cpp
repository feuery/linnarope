#include <tmx_private.h>
#include <project.h>
#include <cassert>
#include <vector>

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

std::vector<Script*>& Project::get_scripts() {
  static std::vector<Script*> vec;

  vec.clear();
  vec.resize(scripts.size());
  std::transform(scripts.begin(), scripts.end(), vec.begin(), [](auto &pair) { return &pair.second; });

  return vec;
}

void Project::insertPalette(int id, std::string name, Palette scr) {
  palettes[name] = scr;
  id_to_palettes[id] = &palettes[name];
}

Palette &Project::getPalette(const char *nme) {
  assert(!palettes.empty());
  return palettes.at(nme);
}
Palette &Project::getPalette(int id) {
  assert(!id_to_palettes.empty());
  Palette *scr = id_to_palettes.at(id);
  return *scr;
}


void Project::insertLisp_Sprite(int id, std::string name, Lisp_sprite scr) {
  lisp_sprites[name] = scr;
  printf("Read sprite %s\n", name.c_str());
  assert(!lisp_sprites[name].pixels.empty());
  id_to_lisp_sprites[id] = &lisp_sprites[name];
}

Lisp_sprite &Project::getLisp_Sprite(const char *nme) {
  assert(!lisp_sprites.empty());
  try {
  return lisp_sprites.at(nme);
  }
  catch(std::out_of_range e) {
    printf("Didn't find sprite called %s. Valid spritenames follow: \n", nme);
    for(auto &pair: lisp_sprites) {
      printf("%s, ", pair.first.c_str());
    }
    printf("\n");
    throw e;
  }
}

Lisp_sprite &Project::getLisp_Sprite(int id) {
  assert(!id_to_lisp_sprites.empty());
  Lisp_sprite *scr = id_to_lisp_sprites.at(id);
  return *scr;
}
