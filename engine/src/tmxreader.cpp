#include <cassert>
#include <cstring>
#include <pugixml.hpp>
#include <cstdio>
#include <string>
#include <vector>
#include <SDL_image.h>
#include "tmxreader.h"
#include "SDL_rect.h"
#include "SDL_surface.h"
#include "tmx_private.h"
#include <libgen.h>

namespace feuertmx {

  Tile unwrap_tile_id(unsigned int tile) {

    const unsigned FLIPPED_HORIZONTALLY_FLAG  = 0x80000000;
    const unsigned FLIPPED_VERTICALLY_FLAG    = 0x40000000;
    const unsigned FLIPPED_DIAGONALLY_FLAG    = 0x20000000;
    const unsigned ROTATED_HEXAGONAL_120_FLAG = 0x10000000;

    bool horizontalFlip = tile & FLIPPED_HORIZONTALLY_FLAG,
      verticalFlip = tile & FLIPPED_VERTICALLY_FLAG;
    
    unsigned int globalID  = tile & ~(  FLIPPED_HORIZONTALLY_FLAG
				       | FLIPPED_VERTICALLY_FLAG
				       | FLIPPED_DIAGONALLY_FLAG
				       | ROTATED_HEXAGONAL_120_FLAG); //clear the flags

    // if(horizontalFlip || verticalFlip) 
    //   printf("unwrapped tile %u into globalid %u, flipped horizontally: %s, flipped vertically: %s \n",
    // 	     tile,
    // 	     globalID,
    // 	     horizontalFlip? "true":"false",
    // 	     verticalFlip? "true":"false");


    return Tile { globalID, horizontalFlip, verticalFlip};
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
    std::vector<Tile> row(expected_w);
    int x, y;
    x = y = 0;

    assert( csv.length() > 0 );

    // .at(0) is a '\n'
    for(int i = 1; i < csv.length(); i++) {
      auto ch = csv.at(i);
      if (ch == ',') {
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
	row = std::vector<Tile>(expected_w);

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
    
    std::string source = tileset_el.attribute("source").as_string(),
      img_path = basepath + "/" + source;
    src_surface = IMG_Load(img_path.c_str());

    for(SDL_Surface *s: linear_tile_surfaces) {
      SDL_FreeSurface (s);
    }
    
    tile_surfaces.clear();
    linear_tile_surfaces.clear();

    for(int x = 0; x < src_surface->w / tilewidth; x++) {
      std::vector<SDL_Surface*> row;
      for(int y = 0; y < src_surface->h / tileheight; y++) {
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
    for(auto &tileset: map->tilesets) {
      tileset.load_source(basepath);
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
    m->width = map_element.attribute("width").as_int();
    m->height = map_element.attribute("height").as_int();
    m->tilewidth = map_element.attribute("tilewidth").as_int();
    m->tileheight = map_element.attribute("tileheight").as_int();

    m->infinite = map_element.attribute("infinite").as_int() == 1;
    m->nextlayerId = map_element.attribute("nextlayerId").as_int();
    m->nextobjectid = map_element.attribute("nextobjectid").as_int();


    auto tilesets = map_element.children("tileset");

    for(auto &tileset_element: tilesets) {
      Tileset t;
      t.firstgid = tileset_element.attribute("firstgid").as_int();
      t.source = tileset_element.attribute("source").as_string();
      t.name = tileset_element.attribute("name").as_string();
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
      for(pugi::xml_node &chunk_element: chunks) {
	LayerChunk chunk;
	
	chunk.x = chunk_element.attribute("x").as_int();
	chunk.y = chunk_element.attribute("y").as_int();
	chunk.width = chunk_element.attribute("width").as_int();
	chunk.height = chunk_element.attribute("height").as_int();

	auto csv_layer = chunk_element.child_value();
	chunk.tiles = parse_layer_csv_data(csv_layer, chunk.width, chunk.height);

	l.chunks.push_back(chunk);
      }
    }

    std::string basepath(dirname(strdup(path)));
    load_tileset_sdl_surfaces(basepath, m);

    return m;
  }

  void delete_map(Map *m) {
    delete m;
  }    
}
