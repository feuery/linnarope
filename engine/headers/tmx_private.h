#pragma once

#include <vector>
#include <SDL.h>
#include <unordered_map>

#include <tmxreader.h>
#include <sqlite3.h>

class Script {
public:
  Script() {}
  Script(Script &scr);
  Script(std::string &nme, std::string &scr);
  std::string name, script;
};

class Project {
public:
  std::vector<Map> maps;
  // id->script
  std::unordered_map<int, Script> scripts;
};

class Tile {
 public:
  /* there should be no > SIGNED_INT_MAX ids at this point */
  int GlobalID;

  bool flipped_horizontally, flipped_vertically;

  Tile (int gid, bool fhor, bool fver);
  Tile (const Tile& t);
 private: Tile ();
};

class Tileset {
 public:
  int firstgid;
  const char *name, *source_attribute;

  std::string tsx_contents;

  void load_tsx_contents(sqlite3 *db);
  void load_source(sqlite3 *db, std::string &document);
  std::tuple<int, const void*> load_img_binary(sqlite3* db, std::string &image_filename);

  int tilewidth, tileheight, tilecount, columns;

  SDL_Surface *src_surface;
  std::vector<std::vector<SDL_Surface*>> tile_surfaces;
  std::vector<SDL_Surface*> linear_tile_surfaces;

  SDL_Surface* tileAt(int x, int y);
  SDL_Surface* tileAt(int local_id);

  ~Tileset();
};

enum Encoding{
  CSV, 
  // not yet supporting any other encodings
  EncodingError
};

class LayerChunk {
 public:
  int x, y, width, height;
  /* a 2d array limited by the width and height fields */
  std::vector<std::vector<Tile>> tiles;
};

class Layer {
 public:
  int id, width, height;
  const char *name;
  Encoding enc;
  std::vector<LayerChunk> chunks;
};

class Object {
 public:
  int id;
  const char *name;
  double x, y, width, height;

  virtual Map* warpzone_dst_map(Project*);
  virtual void render(SDL_Surface *dst, Map *m) = 0;

  Object(Object &o);
  Object() {}
};

class EllipseObject: public Object {
 public:

  // warpzone specific coordinates
  int dst_x, dst_y, dst_map_id;
  Project *proj;
  
  Map* warpzone_dst_map(Project*) override;
  
  void render(SDL_Surface *dst, Map *m) override;
  EllipseObject() {}
};

class BoxObject: public Object {
 public:    
  void render(SDL_Surface *dst, Map *m) override;

  BoxObject() {}
};

class ImageObject: public Object {
 public:
  int gid;

  void render(SDL_Surface *dst, Map *m);

  ImageObject(ImageObject &io);
  ImageObject() {}
};

class ObjectGroup {
 public:
  int id;
  const char *name;
  std::vector<Object*> objs;
};
  
class Map {
 public:    
  double version;
  const char *tiledversion, *orientation, *renderorder;
  int width, height, tilewidth, tileheight, databaseID;
  bool infinite;
  int nextlayerId, nextobjectid;
  std::vector<Tileset*> tilesets;
  std::vector<Layer> layers;
  std::vector<ObjectGroup> objs;
  
  // These should be cleaned up by ~ObjectGroup, if such a function existed
  std::vector<EllipseObject*> warpzones;

  SDL_Surface *rendered_map;
  SDL_Texture *rendered_map_tex;

  // in px, where this map is supposed to be rendered to
  int x,y;

  void renderMap(SDL_Renderer*);
  SDL_Surface* tileAt(int globalID);

  // this should be an address to project's script hash table
  int entry_script_id;
  bool entry_script_found;
  Script *entry_script;

  ~Map();
};

std::variant<bool, Map> read_map(const char *tmx_data, int map_id, std::variant<int, bool> entry_script_id, sqlite3 *db, Project *proj);
