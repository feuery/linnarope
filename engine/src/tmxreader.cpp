#include <cassert>
#include <cstring>
#include <pugixml.hpp>
#include <cstdio>
#include <string>
#include <variant>
#include <vector>
#include <SDL.h>
#include <SDL_image.h>
#include "tmxreader.h"
#include "tmx_private.h"
#include <sqlite3.h>

Script::Script(Script &scr) : name(scr.name), script(scr.script) {}
Script::Script(std::string &nme, std::string &scr): name(nme), script(scr) {}

Tile::Tile(const Tile &t) {
  this->GlobalID = t.GlobalID;
  this->flipped_horizontally = t.flipped_horizontally;
  this->flipped_vertically = t.flipped_vertically;
}

Tile::Tile() {
  this->GlobalID = -666;
  this->flipped_horizontally = false;
  this->flipped_vertically = false;
}

Tile::Tile (int gid, bool fhor, bool fver) {
  this->GlobalID = gid;
  this->flipped_horizontally = fhor;
  this->flipped_vertically = fver;
}
    

Tile unwrap_tile_id(unsigned int tile) {

  const unsigned FLIPPED_HORIZONTALLY_FLAG  = 0x80000000;
  const unsigned FLIPPED_VERTICALLY_FLAG    = 0x40000000;
  const unsigned FLIPPED_DIAGONALLY_FLAG    = 0x20000000;
  const unsigned ROTATED_HEXAGONAL_120_FLAG = 0x10000000;

  bool horizontalFlip = tile & FLIPPED_HORIZONTALLY_FLAG,
    verticalFlip = tile & FLIPPED_VERTICALLY_FLAG;
    
  int globalID  = tile & ~(  FLIPPED_HORIZONTALLY_FLAG
			     | FLIPPED_VERTICALLY_FLAG
			     | FLIPPED_DIAGONALLY_FLAG
			     | ROTATED_HEXAGONAL_120_FLAG); //clear the flags

  // if(horizontalFlip || verticalFlip) 
  //   printf("unwrapped tile %u into globalid %u, flipped horizontally: %s, flipped vertically: %s \n",
  // 	     tile,
  // 	     globalID,
  // 	     horizontalFlip? "true":"false",
  // 	     verticalFlip? "true":"false");


  Tile t( globalID, horizontalFlip, verticalFlip);
  return t;
}

template <typename T>
void transpose(std::vector<std::vector<T>> &arr) {
  for (int x = 0; x < arr.size(); x++) {
    for (int y = 0; y < x; y++) {
      T tmp = arr[x][y];
      arr[x][y] = arr[y][x];
      arr[y][x] = tmp;	
    }
  }
}
    

// this function tries to parse tiled's csv layers into something useful without
// completely exploding the universe. It tries to validate that the dimensions of the
// csv data match what is advertised as <chunk>'s width and height in the xml. If they
// do not match, this throws something stupid like const char* or std::exception  
std::vector<std::vector<Tile>> parse_layer_csv_data(const char *csv_data,
						    int expected_w,
						    int expected_h) {
  std::string csv(csv_data);
  std::string number_acc;
  std::vector<std::vector<Tile>> map;
  std::vector<Tile> row;
  int x, y;
  x = y = 0;

  assert( csv.length() > 0 );

  // .at(0) is a '\n'
  for(int i = 1; i < csv.length(); i++) {
    auto ch = csv.at(i);
    if (ch == ',' || (ch == '\n' && number_acc != "")) {
      try {
	unsigned int num = std::stoul(number_acc);
	Tile t = unwrap_tile_id(num);
	  
	number_acc = "";
	row.push_back(t);
	x++;
      }
      catch(...) {
	printf("number_acc: %s\n", number_acc.c_str());
	throw "";
      }
    }

    if (ch == '\n') {
      y++;
	
      assert( x == expected_w ||
	      // the last line in csv 
	      (y == expected_h
	       // the last line doesn't end in a comma, so it's gonne be one smaller than the previous lines
	       && x == expected_w - 1));
      map.push_back(row);
      x = 0;
      row = std::vector<Tile>();

    }
    else if ( ch >= '0' && ch <= '9') {
      number_acc += ch;
    }
    else if (ch == ',') {
      // nop, just swallow the comma from the else { } error handler
    }
    else {
      fprintf(stderr, "Unrecognized csv character: %c .at(%d)\n", ch, i);
      throw "";
    }
  }

  assert ( y == expected_h );

  transpose(map);

  return map;
}

SDL_Surface* Tileset::tileAt(int x, int y) {
  // wonder if these x/y should be the other way around
  return tile_surfaces.at(x).at(y);
}

SDL_Surface* Tileset::tileAt(int local_id) {
  return linear_tile_surfaces.at(local_id);
}

std::tuple<int, const void*> Tileset::load_img_binary(sqlite3* db, std::string &image_filename) {
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

void* cpy (const void* ptr, int size) {
  unsigned char *bfr = new unsigned char[size];

  for(int i=0; i < size; i++) {
    (*(bfr + i)) = (*(reinterpret_cast<const unsigned char*>(ptr) + i));
  }

  return bfr;
}

void Tileset::load_source(sqlite3 *db, std::string &document) {
  pugi::xml_document tsx;
  auto result = tsx.load_string(document.c_str());

  if(! result) {
    printf("Loading a tileset source from \"%s\" failed \n", document.c_str());
    throw "";
  }

  auto tileset_el = tsx.child("tileset");

  tilewidth = tileset_el.attribute("tilewidth").as_int();
  tileheight = tileset_el.attribute("tileheight").as_int();
  tilecount = tileset_el.attribute("tilecount").as_int();
  columns = tileset_el.attribute("columns").as_int();

  auto image_el = tileset_el.child("image");
    
  std::string imgsource = image_el.attribute("source").as_string();
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
    throw "";
  }
  
  src_surface = IMG_Load_RW(ops, 1);
  if(!src_surface) {
    printf("IMG_Load_RW failed %s\n", SDL_GetError());
    throw "";
  }
    
  delete[] (reinterpret_cast<unsigned char*>(bfr));
  
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
}

int tmxpath_to_id(const char *path, sqlite3 *db) {
  sqlite3_stmt *stmt;
  sqlite3_prepare(db, "SELECT ID FROM Map WHERE tmx_path = ?", -1, &stmt, nullptr);
  sqlite3_bind_text(stmt, 1, path, -1, SQLITE_STATIC);

  if(sqlite3_step(stmt) == SQLITE_DONE) return -1;

  int mapid = sqlite3_column_int(stmt, 0);
  printf("tmx %s => id %d\n", path, mapid);
  sqlite3_finalize(stmt);

  return mapid;
}

void read_scripts(Project *proj, sqlite3 *db) {
  sqlite3_stmt *stmt;
  std::string q = "SELECT id, name, script FROM script";
  int res = sqlite3_prepare_v2(db, q.c_str(), q.size(), &stmt, nullptr);
  if(res == SQLITE_ERROR) {
    printf("read_scripts failed %s\n", sqlite3_errmsg(db));
    throw "";
  }

  while((res = sqlite3_step(stmt)) == SQLITE_ROW) {
    int id = sqlite3_column_int(stmt, 0);
    std::string name = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1)),
      script = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));

    Script scr(name, script);    
    proj->scripts[id] = scr;
  }

  sqlite3_finalize(stmt);
}

Project* read_project(const char *path) {
  sqlite3 *db = nullptr;
  Project *project = new Project;

  auto result = sqlite3_open(path, &db);

  if (result != SQLITE_OK) {
    printf("Opening %s failed due to %s\n", path, sqlite3_errmsg(db));
    throw "";
  }

  printf("Opened project file %s\n", path);

  sqlite3_stmt *stmt;
  std::string map_q = "SELECT id, tmx_file FROM map";
  sqlite3_prepare_v2(db, map_q.c_str(), map_q.size(), &stmt, nullptr);

  int row_result = sqlite3_step(stmt);

  if (row_result == SQLITE_ERROR) {
    printf("Fetching maps failed %s\n", sqlite3_errmsg(db));
    throw "";
  }
  
  do {
    int id = sqlite3_column_int(stmt, 0);
    const char *tmx_blob = reinterpret_cast<const char*>(sqlite3_column_blob(stmt, 1));
    int size = sqlite3_column_bytes(stmt, 1);

    assert(size > 0);
    assert(tmx_blob);

    std::string f;
    f.assign(tmx_blob, size);
    
    std::variant<bool, Map> map_result = read_map(f.c_str(), id, db, project);
    try {
      project->maps.push_back(std::get<Map>(map_result));
      puts("Loaded a map \n");
    }
    catch(std::bad_variant_access &ex) {
      puts("Loading map seems to have failed\n");
      return nullptr;
    }
  } while((row_result = sqlite3_step(stmt)) == SQLITE_ROW);

  read_scripts(project, db);

  assert(!project->scripts.empty());
  printf("Sizeof scripts: %zu\n", project->scripts.size());

  sqlite3_finalize(stmt);
  sqlite3_close_v2(db);

  return project;
}

 void Tileset::load_tsx_contents(sqlite3 *db) {
   sqlite3_stmt *stmt;
   std::string q = "SELECT tsx_contents FROM tileset WHERE filename = ?";
   int res = sqlite3_prepare_v2(db, q.c_str(), q.size(), &stmt, nullptr);

   if (res != SQLITE_OK) {
     printf("Load tsx_contents failed %s\n", sqlite3_errmsg(db));
     throw "";
   }

   sqlite3_bind_text(stmt, 1, source_attribute, -1, SQLITE_STATIC);

   res = sqlite3_step(stmt);
   assert(res == SQLITE_ROW);

   this->tsx_contents = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
   sqlite3_finalize(stmt);
 }


std::variant<bool, Map> read_map(const char *tmx_data, int map_id, sqlite3 *db, Project *proj) {
  assert(tmx_data);
  pugi::xml_document doc;
  pugi::xml_parse_result result = doc.load_string(tmx_data);  

  if(!result) {
    fprintf(stderr, "Couldn't load map\n");
    return false;
  }

  auto map_element = doc.child("map");
  Map m;

  m.databaseID = map_id;
  m.version = map_element.attribute("version").as_double();

  m.tiledversion = map_element.attribute("tiledversion").as_string();
  m.orientation = map_element.attribute("orientation").as_string();
  m.renderorder = map_element.attribute("renderorder").as_string();
  // these are a lie when infinite-flag is true 
  m.width = map_element.attribute("width").as_int();
  m.height = map_element.attribute("height").as_int();
  m.tilewidth = map_element.attribute("tilewidth").as_int();
  m.tileheight = map_element.attribute("tileheight").as_int();

  m.infinite = map_element.attribute("infinite").as_int() == 1;
  m.nextlayerId = map_element.attribute("nextlayerId").as_int();
  m.nextobjectid = map_element.attribute("nextobjectid").as_int();

  auto tilesets = map_element.children("tileset");

  for(auto &tileset_element: tilesets) {
    Tileset *t = new Tileset;
    t->firstgid = tileset_element.attribute("firstgid").as_int();
    t->source_attribute = tileset_element.attribute("source").as_string();
    t->name = tileset_element.attribute("name").as_string();
    t->load_tsx_contents(db);
    t->load_source(db, t->tsx_contents);
    m.tilesets.push_back(t);
  }

  auto layers = map_element.children("layer");
  for(auto &layer_element: layers) {
    Layer l;
    l.id = layer_element.attribute("id").as_int();
    l.height = layer_element.attribute("height").as_int();
    l.width = layer_element.attribute("width").as_int();
    l.name = layer_element.attribute("name").as_string();

    auto data = layer_element.child("data");
    Encoding encoding = std::string(data.attribute("encoding").as_string()) == "csv" ? CSV: EncodingError;

    assert(encoding == CSV);

    auto chunks = data.children("chunk");

    if(chunks.empty()) {
      LayerChunk c;
      c.x = 0;
      c.y = 0;
      c.width = l.width;
      c.height = l.height;
	
      auto csv_layer = data.child_value();
      c.tiles = parse_layer_csv_data(csv_layer, c.width, c.height);
      l.chunks.push_back(c);
    }
    else {
      for(pugi::xml_node &chunk_element: chunks) {
	LayerChunk chunk;
	
	chunk.x = chunk_element.attribute("x").as_int();
	chunk.y = chunk_element.attribute("y").as_int();
	chunk.width = chunk_element.attribute("width").as_int();
	chunk.height = chunk_element.attribute("height").as_int();

	auto csv_layer = chunk_element.child_value();
	chunk.tiles = parse_layer_csv_data(csv_layer, chunk.width, chunk.height);

	l.chunks.push_back(chunk);
      }}

    m.layers.push_back(l);
  }

  auto objgroups = map_element.children("objectgroup");
  for(auto &objgroup: objgroups) {
    auto objs = objgroup.children("object");

    ObjectGroup ogroup;
    ogroup.id = objgroup.attribute("id").as_int();
    ogroup.name = objgroup.attribute("name").as_string();
      
    for(auto &obj: objs) {
      auto gid_attribute = obj.attribute("gid");
      auto ellipse = obj.child("ellipse");

      Object *o = gid_attribute? new ImageObject:
	ellipse? static_cast<Object*>(new EllipseObject): static_cast<Object*>(new BoxObject);

      o->id = obj.attribute("id").as_int();
      o->x = obj.attribute("x").as_double();
      o->y = obj.attribute("y").as_double();
      o->width = obj.attribute("width").as_int();
      o->height = obj.attribute("height").as_int();

      if (gid_attribute) {
	ImageObject *io = static_cast<ImageObject*>(o);
	io->gid = obj.attribute("gid").as_int();
      }

      if(ellipse && db) {
	EllipseObject *eo = static_cast<EllipseObject*>(o);
		
	// mapid is generated by sqlite, and there is nothing more unique to be generated from a map other than it's path
	// which should probably be set up as UNIQUE in sqlite too...
	int map_id = m.databaseID;
	assert(map_id>=0);

	printf("map id %d, o id %d\n", map_id, eo->id);

	sqlite3_stmt *stmt;
	
	// a potential warpzone, let's see if sqlite knows of a destination map
	sqlite3_prepare(db, "SELECT m.tmx_path, m.ID, o.x, o.y \
FROM warp_connection wc \
JOIN object o ON wc.dst_warpzone = o.internal_id \
JOIN map m ON m.ID = wc.dst_map \
JOIN object src_o ON wc.src_warpzone = src_o.internal_id \
WHERE wc.src_map = ? AND src_o.id = ?", -1, &stmt, nullptr);
        sqlite3_bind_int(stmt, 1, map_id);
        sqlite3_bind_int(stmt, 2, eo->id);

	printf("Loading with eo->id of %d\n", eo->id);

	int count_of_kids = 0;

	auto step_result = sqlite3_step(stmt);
	printf("step_result: %d\n", step_result);
	while(step_result == SQLITE_ROW) {	  
	  int dst_map_id = sqlite3_column_int(stmt, 1),
	    dst_x = sqlite3_column_int(stmt, 2),
	    dst_y = sqlite3_column_int(stmt, 3);

	  count_of_kids++;

	  printf("dst_map id, x, y: %d, %d, %d\n", dst_map_id, dst_x, dst_y);

	  auto c_tmxpath = sqlite3_column_text(stmt, 0);
	  assert(c_tmxpath);
	  std::string dst_tmxpath(reinterpret_cast<const char*>(c_tmxpath));

	  // x and y are in the destination map's "model space"
	  eo->dst_x = dst_x;
	  eo->dst_y = dst_y;
	  eo->dst_map_id = dst_map_id;
	  eo->proj = proj;
	  
	  m.warpzones.push_back(eo);

	  // TODO move back to while() initialization list
	  step_result = sqlite3_step(stmt);
	}

	printf("Loaded %d child maps\n", count_of_kids);
	sqlite3_finalize(stmt);
      }

      ogroup.objs.push_back(o);
      m.objs.push_back(ogroup);
    }
  }

  if (m.infinite) {

    // we don't actually support infinite maps, so... hopefully this works :D
      
    std::vector<int> xs, ys;
    for(auto &l: m.layers)
      for(auto &c: l.chunks) {
	xs.push_back(c.x);
	xs.push_back(c.x + c.width);

	ys.push_back(c.y);
	ys.push_back(c.y + c.height);
      }

    auto horizontal = std::ranges::minmax(xs),
      vertical = std::ranges::minmax(ys);

    m.width = horizontal.max - horizontal.min;
    m.height = vertical.max - horizontal.min;      
  }
    
  return m;
}

Tileset::~Tileset() {
}

Map::~Map() {
}

void delete_map(Map *m) {
  delete m;
}

void Map::renderMap(SDL_Renderer *r) {
  // yolo what a deref 
  auto format = tilesets.at(0)->linear_tile_surfaces.at(0)->format;
    
  std::vector<int> xs, ys;

  printf("%zu layers\n", layers.size());
  for(auto &l: layers) {
    printf("%zu chunks\n", l.chunks.size());
    for(auto &c: l.chunks) {
      xs.push_back(c.x);
      ys.push_back(c.y);
    }
  }

  // auto lowhigh_x = std::ranges::minmax(xs),
  //   lowhigh_y = std::ranges::minmax(ys);
    
  auto rmask = format->Rmask,
    gmask = format->Gmask,
    bmask = format->Bmask,
    amask = format->Amask;

  int w = width * tilewidth,
    h = height * tileheight;

  printf("Trying to create map with dimensions %d, %d\n", w, h);

  SDL_Surface *dst = SDL_CreateRGBSurface(0,
					  w, h,
					  32,
					  rmask, gmask, bmask, amask);

  assert(dst);
    
  assert(layers.size() > 0);
  for(int l = 0; l < layers.size(); l++) {
    auto &chunks = layers.at(l).chunks;
    for (auto &chunk: chunks) {
      for (int x = chunk.x; x < chunk.x + chunk.width; x++)
	for (int y = chunk.y; y  < chunk.y + chunk.height; y++) {	    
	  Tile& tile = chunk.tiles.at( x - chunk.x).at(y - chunk.y);
	  SDL_Surface *tile_src = tileAt(tile.GlobalID);
	    
	  if(! tile_src) {
	    // printf("Searching chunked tile data at %d, %d\n", chunkX, chunkY);
	    continue;
	  }

	  SDL_Rect dst_rect { (x * tilewidth), y * tileheight, tilewidth, tileheight};
	  auto blit_result = SDL_BlitSurface(tile_src, nullptr, dst, &dst_rect);

	  assert(blit_result == 0);
	}
    }
  }

  for( auto &ogroup: objs ) {
    for ( auto *obj: ogroup.objs ) {
      obj->render(dst, this);
    }
  }

  rendered_map = dst;
  rendered_map_tex = SDL_CreateTextureFromSurface(r, dst);

  if(!rendered_map_tex) {
    fprintf(stderr, "Making a texture of the map failed: %s\n", SDL_GetError());
  }
    
  assert (rendered_map_tex);
}

SDL_Surface* Map::tileAt(int globalId) {
  if (globalId == 0 ) return nullptr;

  assert(! tilesets.empty());
  std::vector<Tileset*> tsets;
    
  for(Tileset *tileset: tilesets) {
    if(tileset->firstgid <= globalId) tsets.push_back(tileset);
  }

  assert(! tsets.empty());

  Tileset *highest = tsets.at(0);

  for(auto tset: tsets) {
    if(tset->firstgid > highest->firstgid) highest = tset;
  }

  return highest->tileAt(globalId - highest->firstgid);
}

void render_map(Map *m, SDL_Renderer *r) {
  if(! m->rendered_map ) {
    puts("Rendering the map");
    m->renderMap(r);

    printf("coords: %d, %d; size: %d, %d\n", m->x, m->y, m->rendered_map->w, m->rendered_map->h);
  }

  if(! m->rendered_map_tex) {
    fputs("Map texture is null. This is bad.\n", stderr);
    throw "";
  }

  SDL_Rect location { m->x, m->y, m->rendered_map->w, m->rendered_map->h};
  // printf("Drawing a texture with params { %d, %d, %d, %d}\n",  m->x, m->y, m->rendered_map->w, m->rendered_map->h);
  SDL_RenderCopy(r, m->rendered_map_tex, NULL, &location); 
    
}



void BoxObject::render(SDL_Surface *dst, Map *m) {

}

void EllipseObject::render(SDL_Surface *dst, Map *m) {}

Map* EllipseObject::warpzone_dst_map(Project *proj)
{
  for(auto &map: proj->maps) {
    if (map.databaseID == dst_map_id) {
      return &map;
    }
  }
  return nullptr;
}
    

Map* Object::warpzone_dst_map(Project *proj) {
  return nullptr;
}

void ImageObject::render(SDL_Surface *dst, Map *m) {
  auto tile = m->tileAt(gid);

  // tiled x is the same as sdl x
  int xx = static_cast<int>(floor(x)),
    // tiled y seems to be at the bottom of the object???
    yy = static_cast<int>(floor(y - height)),
    ww = static_cast<int>(floor(width)),
    hh = static_cast<int>(floor(height));

  SDL_Rect dst_rect { xx, yy, ww, hh };
  auto blit_result = SDL_BlitSurface(tile, nullptr, dst, &dst_rect);

  assert(blit_result == 0);    
}

Object::Object(Object &o) {
  id = o.id;
  name = o.name;
  x = o.x;
  y = o.y;
  width = o.width;
  height = o.height;
}

ImageObject::ImageObject(ImageObject &io): Object(io) {
  gid = io.gid;
}

SDL_Surface* map_surface(Map* m) {
  return m->rendered_map;
}

int map_w(Map *m) { return m->width; }

int map_h(Map *m) { return m->height;}

void generate_drawing_context(Project *proj, Map *m, drawing_state *ctx, SDL_Renderer *r, std::vector<Map*>* visited_maps, int depth, xy parent_map_location, xy parent_warpzone_location, xy dst_warpzone_location)
{
  if(! visited_maps) visited_maps = new std::vector<Map*>;

  // bool first_map = depth == 1;

  int current_map_x = parent_map_location.x + parent_warpzone_location.x - dst_warpzone_location.x,
    current_map_y = parent_map_location.y + parent_warpzone_location.y - dst_warpzone_location.y;

  if(!m->rendered_map) {
    m->renderMap(r);
  }

  assert(m->rendered_map);

  draw(ctx,
       current_map_x,
       current_map_y,
       m->rendered_map->w,
       m->rendered_map->h,
       m->rendered_map);
  
  visited_maps->push_back(m);

  printf("%zu children being drawn\n", m->warpzones.size ());

  for(auto& child: m->warpzones) {
    Map *dst = child->warpzone_dst_map(proj);
    printf("Child %d\n", dst->databaseID);
    // if(std::find(visited_maps->begin(), visited_maps->end(), dst) != visited_maps->end()) continue;

    generate_drawing_context(proj, dst, ctx, r, visited_maps, depth + 1, {current_map_x, current_map_y}, {static_cast<int>(floor( child->x)), static_cast<int>(floor(child->y))}, {child->dst_x, child->dst_y});
  }
}

Map *getMaps(Project *proj, int &count_of_maps) {
  count_of_maps = proj->maps.size();
  return proj->maps.data();
}

void map_x(Map *m, int x) { m->x = x; }

void map_y(Map *m, int y) { m->y = y; }
