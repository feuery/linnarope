#pragma once

#include <string>
#include <unordered_map>
#include <SDL.h>
#include <tmx_private.h>
#include <lisp_sprite.h>

class Project {
private:
  std::unordered_map<std::string, Script> scripts;
  std::unordered_map<int, Script*> id_to_scripts;
  
public:
  std::unordered_map<std::string, Map> maps;
  
  std::unordered_map<std::string, Lisp_sprite> lisp_sprites;
  std::unordered_map<std::string, Palette> palettes;
  std::unordered_map<std::string, Sprite> sprites;

  void insertScript(int id, const char *name, Script scr);
  Script& getScript(const char *name);
  Script& getScript(int id);

  bool hasScript(int id);
};
