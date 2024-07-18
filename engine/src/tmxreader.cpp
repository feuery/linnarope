#include <cassert>
#include <pugixml.hpp>
#include <cstdio>
#include <string>
#include <vector>
#include "tmxreader.h"
#include "tmx_private.h"

namespace feuertmx {

  // this function tries to parse tiled's csv layers into something useful without
  // completely exploding the universe. It tries to validate that the dimensions of the
  // csv data match what is advertised as <chunk>'s width and height in the xml. If they
  // do not match, this throws something stupid like const char* or std::exception  
  std::vector<std::vector<long>> parse_layer_csv_data(const char *csv_data,
						     int expected_w,
						     int expected_h) {
    std::string csv(csv_data);
    std::string number_acc;
    std::vector<std::vector<long>> map;
    std::vector<long> row(expected_w);
    int x, y;
    x = y = 0;

    assert( csv.length() > 0 );

    // .at(0) is a '\n'
    for(int i = 1; i < csv.length(); i++) {
      auto ch = csv.at(i);
      if (ch == ',') {
	try {
	  int num = std::stol(number_acc);
	  number_acc = "";
	  row.push_back(num);
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
	row = std::vector<long>(expected_w);

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

  Map* read_map(const char *path) {
    pugi::xml_document doc;
    pugi::xml_parse_result result = doc.load_file(path);

    if(!result)
      fprintf(stderr, "Couldn't load map %s\n", path);

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

    return m;
  }

  void delete_map(Map *m) {
    delete m;
  }    
}
