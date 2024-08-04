#include <cassert>
#include <cmath>
#include <cstring>
#include <pugixml.hpp>
#include <cstdio>
#include <string>
#include <vector>
#include <SDL.h>
#include <SDL_image.h>
#include "tmxreader.h"
#include "SDL_render.h"
#include "SDL_surface.h"
#include "tmx_private.h"
#include "tmxlite/Layer.hpp"
#include <libgen.h>

namespace feuertmx {

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

  void Tileset::load_source(std::string basepath) {
    pugi::xml_document tsx;
    auto tsx_path = basepath + "/" + source;
    auto result = tsx.load_file(tsx_path.c_str());

    if(! result) {
      printf("Loading a tileset source from %s failed \n", tsx_path.c_str());
      throw "";
    }

    auto tileset_el = tsx.child("tileset");

    tilewidth = tileset_el.attribute("tilewidth").as_int();
    tileheight = tileset_el.attribute("tileheight").as_int();
    tilecount = tileset_el.attribute("tilecount").as_int();
    columns = tileset_el.attribute("columns").as_int();

    auto image_el = tileset_el.child("image");
    
    std::string source = image_el.attribute("source").as_string();
    assert(source != "");
    
    auto img_path = basepath + "/" + source;

    printf("trying to load %s\n", img_path.c_str());					      
    src_surface = IMG_Load(img_path.c_str());

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

  // this function introduces a hard dependency to SDL2 and tries to
  // load tilesets (both tsx and their accompanying pngs) in a way
  // that they could be drawn to a window
  void load_tileset_sdl_surfaces(std::string basepath, Map *map) {
    for(Tileset *tileset: map->tilesets) {
      tileset->load_source(basepath);
    }
  }

  Map* read_map(const char *path) {
    pugi::xml_document doc;
    pugi::xml_parse_result result = doc.load_file(path);

    if(!result) {
      fprintf(stderr, "Couldn't load map %s\n", path);
      return nullptr;
    }

    auto map_element = doc.child("map");
    Map *m = new Map;

    m->version = map_element.attribute("version").as_double();

    m->tiledversion = map_element.attribute("tiledversion").as_string();
    m->orientation = map_element.attribute("orientation").as_string();
    m->renderorder = map_element.attribute("renderorder").as_string();
    // these are a lie when infinite-flag is true 
    m->width = map_element.attribute("width").as_int();
    m->height = map_element.attribute("height").as_int();
    m->tilewidth = map_element.attribute("tilewidth").as_int();
    m->tileheight = map_element.attribute("tileheight").as_int();

    m->infinite = map_element.attribute("infinite").as_int() == 1;
    m->nextlayerId = map_element.attribute("nextlayerId").as_int();
    m->nextobjectid = map_element.attribute("nextobjectid").as_int();


    auto tilesets = map_element.children("tileset");

    for(auto &tileset_element: tilesets) {
      Tileset *t = new Tileset;
      t->firstgid = tileset_element.attribute("firstgid").as_int();
      t->source = tileset_element.attribute("source").as_string();
      t->name = tileset_element.attribute("name").as_string();
      m->tilesets.push_back(t);
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

      m->layers.push_back(l);
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

	o->id = obj.attribute("gid").as_int();
	o->x = obj.attribute("x").as_double();
	o->y = obj.attribute("y").as_double();
	o->width = obj.attribute("width").as_int();
	o->height = obj.attribute("height").as_int();

	if (gid_attribute) {
	  ImageObject *io = static_cast<ImageObject*>(o);
	  io->gid = obj.attribute("gid").as_int();
	}

	ogroup.objs.push_back(o);
	m->objs.push_back(ogroup);
      }
    }

    std::string basepath(dirname(strdup(path)));
    load_tileset_sdl_surfaces(basepath, m);

    if (m->infinite) {

      // we don't actually support infinite maps, so... hopefully this works :D
      
      std::vector<int> xs, ys;
      for(auto &l: m->layers)
	for(auto &c: l.chunks) {
	  xs.push_back(c.x);
	  xs.push_back(c.x + c.width);

	  ys.push_back(c.y);
	  ys.push_back(c.y + c.height);
	}

      auto horizontal = std::ranges::minmax(xs),
	vertical = std::ranges::minmax(ys);

      m->width = horizontal.max - horizontal.min;
      m->height = vertical.max - horizontal.min;      
    }
    
    return m;
  }

  Tileset::~Tileset() {
    // puts("Freeing a ~Tileset");
    for(SDL_Surface *srfc: linear_tile_surfaces) {
      SDL_FreeSurface(srfc);
    }
    SDL_FreeSurface(src_surface);
  }

  Map::~Map() {
    // puts("Freeing a ~map");

    for(Tileset *t: tilesets){
      delete t;
    }
    
    SDL_DestroyTexture(rendered_map_tex);
    SDL_FreeSurface(rendered_map);
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

    int successfully_blitted_tiles = 0;
    int loop_counter = 0;
    
    assert(layers.size() > 0);
    for(int l = 0; l < layers.size(); l++) {
      auto &chunks = layers.at(l).chunks;
      for (auto &chunk: chunks) {
	for (int x = chunk.x; x < chunk.x + chunk.width; x++)
	  for (int y = chunk.y; y  < chunk.y + chunk.height; y++) {
	    loop_counter++;
	    int chunkX = x - chunk.x, chunkY = y - chunk.y;	    
	    
	    Tile& tile = chunk.tiles.at( x - chunk.x).at(y - chunk.y);
	    SDL_Surface *tile_src = tileAt(tile.GlobalID);
	    
	    if(! tile_src) {
	      // printf("Searching chunked tile data at %d, %d\n", chunkX, chunkY);
	      continue;
	    }

	    SDL_Rect dst_rect { (x * tilewidth), y * tileheight, tilewidth, tileheight};
	    auto blit_result = SDL_BlitSurface(tile_src, nullptr, dst, &dst_rect);

	    assert(blit_result == 0);
	    
	    successfully_blitted_tiles++;
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

  void EllipseObject::render(SDL_Surface *dst, Map *m) {

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
}
