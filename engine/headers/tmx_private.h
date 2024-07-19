#pragma once

#include <vector>
#include <SDL.h>

namespace feuertmx {

  class Tile {
  public:
    unsigned int GlobalID;

    bool flipped_horizontally, flipped_vertically;
  };

  class Tileset {
  public:
    int firstgid;
    const char *source, *name;

    void load_source(std::string basepath);

    int tilewidth, tileheight, tilecount, columns;

    SDL_Surface *src_surface;
    std::vector<std::vector<SDL_Surface*>> tile_surfaces;
    std::vector<SDL_Surface*> linear_tile_surfaces;

    SDL_Surface* tileAt(int x, int y);
    SDL_Surface* tileAt(int local_id);
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
  
  class Map {
  public: 
    double version;
    const char *tiledversion, *orientation, *renderorder;
    int width, height, tilewidth, tileheight;
    bool infinite;
    int nextlayerId, nextobjectid;
    std::vector<Tileset> tilesets;
  };
}
