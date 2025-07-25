#pragma once

#include <string>
#include <vector>
#include <SDL.h>

#include <tmxreader.h>
#include <sqlite3.h>
#include <variant>

class Resource {
public:
  virtual const char* get_typename() = 0;
  virtual void render_to_screen(int x, int y);
};

class Script: public Resource {
public:
  Script() {}
  Script(Script &scr);
  Script(std::string &nme, std::string &scr);
  std::string name, script;

  const char* get_typename() override;
};

class Tile: public Resource {
public:
  /* there should be no > SIGNED_INT_MAX ids at this point */
  int GlobalID;

  bool flipped_horizontally, flipped_vertically;

  Tile (int gid, bool fhor, bool fver);
  Tile (const Tile& t);

  virtual ~Tile() = default;
  
  const char* get_typename() override;
private: Tile ();
};

class Tileset: public Resource {
private:
  bool load_xml(std::string& doc);
  bool load_images(const char *basepath);
  bool load_images(sqlite3*);
  bool generate_tile_surfaces();

  // source attribute of a <tileset>'s hopefully only <image> tag
  std::string imgsource;
  int tileset_width, tileset_height;
  
public:
  int firstgid;

  // source_attribute is tsx's filename in tmx
  std::string source_attribute;
  const char *name;

  const char* get_typename() override;

  bool populate_tileset(sqlite3 *);
  bool populate_tileset(const char* basepath);

  
  // std::tuple<int, const void *> load_img_binary(sqlite3 *db,
  //                                               std::string &image_filename);
  // std::tuple<int, const void*> load_img_binary(std::string &image_filename);

  int tilewidth, tileheight, tilecount, columns;

  SDL_Surface *src_surface;
  std::vector<std::vector<SDL_Surface*>> tile_surfaces;
  std::vector<SDL_Surface*> linear_tile_surfaces;

  SDL_Surface* tileAt(int x, int y);
  SDL_Surface* tileAt(int local_id);

  int width();
  int height();

  ~Tileset();
};

enum Encoding{
  CSV, 
  // not yet supporting any other encodings
  EncodingError
};

class LayerChunk: public Resource {
public:
  int x, y, width, height;
  /* a 2d array limited by the width and height fields */
  std::vector<std::vector<Tile>> tiles;

  const char* get_typename() override;
  virtual ~LayerChunk();
};

class Layer: public Resource {
public:
  int id, width, height;
  const char *name;
  Encoding enc;
  std::vector<LayerChunk> chunks;

  const char* get_typename() override;
  virtual ~Layer();
};

class Object: public Resource {
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

  const char* get_typename() override;
};

class BoxObject: public Object {
public:    
  void render(SDL_Surface *dst, Map *m) override;

  BoxObject() {}
  const char* get_typename() override;
};

class ImageObject: public Object {
public:
  int gid;

  void render(SDL_Surface *dst, Map *m) override;

  ImageObject(ImageObject &io);
  ImageObject() {}

  const char* get_typename() override;
};

class ObjectGroup: public Resource {
public:
  int id;
  const char *name;
  std::vector<Object*> objs;

  const char* get_typename() override;

  virtual ~ObjectGroup();
};
  
class Map: public Resource {
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

  std::string name; // filename is calculated by tmx_path ~= s/.tmx$//

  const char* get_typename() override;
  void render_to_screen(int x, int y) override;

  // these loop tilesets through and load corresponding tsx and render them from either db or fs
  void loadTilesets(sqlite3*);
  void loadTilesets(const char *basepath);

  virtual ~Map();
  Map();
};

std::variant<bool, Map> read_map(const char *tmx_data, int map_id, std::variant<int, bool> entry_script_id, sqlite3 *db, Project *proj);
// used in maprenderer tasks 
Map tmx_to_map(const char *tmx_data);


// takes the const out of void* returned by sqlite. 
void* cpy (const void* ptr, int size);
void delete_cpyed(void *bfr);
