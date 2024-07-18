#pragma once

#include <vector>

namespace feuertmx {

  class Tileset {
  public:
    int firstgid;
    const char *source, *name;
    
  };

  enum Encoding{
    CSV, 
    // not yet supporting any else encodings
    EncodingError
  };

  class LayerChunk {
  public:
    int x, y, width, height;
    /* a 2d array limited by the width and height fields */
    std::vector<std::vector<long>> tiles; 		
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
