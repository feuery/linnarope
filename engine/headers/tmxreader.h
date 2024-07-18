#pragma #once

namespace feuertmx {
  class Map;
  Map* read_map(const char *path);
  void delete_map(Map*);
}


