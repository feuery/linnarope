#include <cstdio>
#include <tmx_json.h>

using json = nlohmann::json;

void to_json(json &j, const Tile &t) {
  j = json { {"GlobalID", t.GlobalID},
	     {"flipped_horizontally", t.flipped_horizontally},
	     {"flipped_vertically", t.flipped_vertically}};
}

void to_json(json &j, const LayerChunk &lc) {
  int d1 = lc.tiles.size(),
    d2 = lc.tiles.at(0).size();
  j = json { {"x", lc.x},
	     {"y", lc.y},
	     {"width", lc.width},
	     {"height", lc.height},
	     {"d1-of-tiles", d1},
	     {"d2-of-tiles", d2}};
}

void to_json(json &j, const Layer &l) {
  j = json{ {"id", l.id},
	    {"width", l.width},
	    {"height", l.height},
	    {"name", l.name},
	    {"encoding", l.enc == CSV? "CSV": "ERROR"},
	    {"chunks", l.chunks}};
}

void to_json( json& j, const Map& m) {
  printf("entry script: %p\n", m.entry_script);
  j = json{{"name", m.name},
	   {"version", m.version},
	   {"width", m.width},
	   {"height", m.height},
	   {"tilewidth", m.tilewidth},
	   {"tileheight", m.tileheight},
	   {"databaseID", m.databaseID},
	   {"entry_script_name", m.entry_script ? m.entry_script->name: "0"},
	   {"layers", m.layers}};
}

// void from_json(const nlohmann::json::json& j, Map& p)
//         j.at("name").get_to(p.name);
//         j.at("address").get_to(p.address);
//         j.at("age").get_to(p.age);
//     }

