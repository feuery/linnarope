#include <tmx_private.h>
#include <SDL.h>
#include <cassert>
#include <lib_fixup.h>
#include <pugixml.hpp>
#include <SDL_image.h>

std::tuple<int, const void*> load_img_binary(sqlite3* db, std::string &image_filename);

void delete_cpyed(void *bfr) {
  delete[] (reinterpret_cast<unsigned char*>(bfr));
}
  
void* cpy (const void* ptr, int size) {
  unsigned char *bfr = new unsigned char[size];

  for(int i=0; i < size; i++) {
    (*(bfr + i)) = (*(reinterpret_cast<const unsigned char*>(ptr) + i));
  }

  return bfr;
}

bool Tileset::load_images(const char *basepath)
{
  std::string img_source = std::string(basepath) + "/" + this->imgsource;
  src_surface = IMG_Load(img_source.c_str());
  return generate_tile_surfaces();
}

bool Tileset::load_images(sqlite3 *db) {
  assert(imgsource != "");

  auto [img_size, img_data] = load_img_binary(db, imgsource);

  assert(img_size > 0);
  
  // let's cast the const out of void 
  void *bfr = cpy(img_data, img_size);

  assert(bfr);

  printf("trying to load %s\n", imgsource.c_str());

  SDL_RWops *ops = SDL_RWFromMem(bfr, img_size);

  if(!ops) {
    printf("ops failed %s\n", SDL_GetError());
    return false;
  }
  
  src_surface = IMG_Load_RW(ops, 1);
  if(!src_surface) {
    printf("IMG_Load_RW failed %s\n", SDL_GetError());
    return false;
  }
    
  delete_cpyed(bfr);

  return generate_tile_surfaces();
}

bool Tileset::generate_tile_surfaces () {  
  assert(src_surface);

  for(SDL_Surface *s: linear_tile_surfaces) {
    SDL_FreeSurface (s);
  }
    
  tile_surfaces.clear();
  linear_tile_surfaces.clear();

  for(int y = 0; y < src_surface->h / tileheight; y++) {
    std::vector<SDL_Surface*> row;
    for(int x = 0; x < src_surface->w / tilewidth; x++) {
      SDL_Surface *a_tile = SDL_CreateRGBSurface(0, tilewidth, tileheight, 32,
						 src_surface->format->Rmask,
						 src_surface->format->Gmask,
						 src_surface->format->Bmask,
						 src_surface->format->Amask);
      SDL_Rect src_rect { x * tilewidth, y * tileheight, tilewidth, tileheight};
      SDL_BlitSurface(src_surface, &src_rect, a_tile, nullptr);
      row.push_back(a_tile);
      linear_tile_surfaces.push_back(a_tile);
    }
    tile_surfaces.push_back(row);
  }

  return true;
}

bool Tileset::load_xml(std::string &document) {
  pugi::xml_document tsx;
  auto result = tsx.load_string(document.c_str());

  if(! result) {
    printf("Loading a tileset source from \"%s\" failed \n", document.c_str());
    return false;
  }

  auto tileset_el = tsx.child("tileset");

  tilewidth = tileset_el.attribute("tilewidth").as_int();
  tileheight = tileset_el.attribute("tileheight").as_int();
  tilecount = tileset_el.attribute("tilecount").as_int();
  columns = tileset_el.attribute("columns").as_int();

  auto image_el = tileset_el.child("image");
    
  imgsource = image_el.attribute("source").as_string();
  return true;
}

bool Tileset::populate_tileset(const char* basepath) {
  std::string path = std::string(basepath) + "/" + source_attribute;
  std::string tsx_contents = get_file_contents(path.c_str());

  return load_xml(tsx_contents) && load_images(basepath);
}

bool Tileset::populate_tileset(sqlite3 *db) {
  sqlite3_stmt *stmt;

  assert(source_attribute != "");

  printf("Trying to populate tileset %s\n", source_attribute.c_str());
  
  std::string q = "SELECT tsx_contents FROM tileset WHERE filename = ?";
  int res = sqlite3_prepare_v2(db, q.c_str(), q.size(), &stmt, nullptr);

  if (res != SQLITE_OK) {
    printf("Load tsx_contents failed %s\n", sqlite3_errmsg(db));
    throw "";
  }

  sqlite3_bind_text(stmt, 1, source_attribute.c_str(), -1, SQLITE_STATIC);

  res = sqlite3_step(stmt);
  assert(res == SQLITE_ROW);

  std::string tsx_contents(reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0)));
  sqlite3_finalize(stmt);

  return load_xml(tsx_contents) && load_images(db);
}

SDL_Surface* Tileset::tileAt(int x, int y) {
  // wonder if these x/y should be the other way around
  return tile_surfaces.at(x).at(y);
}

SDL_Surface* Tileset::tileAt(int local_id) {
  return linear_tile_surfaces.at(local_id);
}

// loads sizeof and the raw blob bytes of a png from sqlite3 
std::tuple<int, const void*> load_img_binary(sqlite3* db, std::string &image_filename) {
  sqlite3_stmt *stmt;
  std::string q = "SELECT img FROM image_file WHERE filename = ?";
  int result = sqlite3_prepare_v2(db, q.c_str(), q.size(), &stmt, nullptr);
  assert(result == SQLITE_OK);

  sqlite3_bind_text(stmt, 1, image_filename.c_str(), image_filename.size(), SQLITE_STATIC);
  result = sqlite3_step(stmt);
  assert (result == SQLITE_ROW);

  const void* blob = sqlite3_column_blob(stmt, 0);
  int _sizeof = sqlite3_column_bytes(stmt, 0);

  assert(_sizeof > 0);

  sqlite3_finalize(stmt);

  return {_sizeof, blob};
}

Tileset::~Tileset() {}

const char* Tileset::get_typename() { return "Tileset"; }
