#pragma once

#include <string>
#include <unordered_map>
#include <SDL.h>
#include <tmx_private.h>
#include <sprite.h>
#include <lisp_sprite.h>
#include <vector>

class Project {
private:
  std::unordered_map<std::string, Script> scripts;
  std::unordered_map<std::string, Palette> palettes;
  std::unordered_map<std::string, Lisp_sprite> lisp_sprites;

  std::unordered_map<int, Script*> id_to_scripts;
  std::unordered_map<int, Palette*> id_to_palettes;
  std::unordered_map<int, Lisp_sprite*> id_to_lisp_sprites;
  
public:

  std::vector<Script*>& get_scripts();
  std::unordered_map<std::string, Map> maps;
  std::unordered_map<std::string, Sprite> sprites;

  void insertPalette(int id, std::string nme, Palette p);
  Palette& getPalette(const char *name);
  Palette& getPalette(int id);

  void insertLisp_Sprite(int id, std::string nme, Lisp_sprite p);
  Lisp_sprite& getLisp_Sprite(const char *name);
  Lisp_sprite& getLisp_Sprite(int id);

  void insertScript(int id, const char *name, Script scr);
  Script& getScript(const char *name);
  Script& getScript(int id);

  bool hasScript(int id);
};
