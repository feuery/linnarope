#include <tmxreader.h>
#include <project.h>
#include <nlohmann/json.hpp>
#include <tmx_json.h>
#include <catch2/catch_test_macros.hpp>


TEST_CASE("load example tmx") {
  Project *proj = nullptr;
  proj = read_project("./test-resources/linnarope-export.game");
  // Project is loaded correctly with enough maps 
  REQUIRE(proj);
  REQUIRE(proj->maps.size() == 2);

  // Maps called the same as in the resource handler

  auto iter = proj->maps.find("pikkustadi-toolon tulli.2.tmx");
  REQUIRE(iter != proj->maps.end());

  iter = proj->maps.find("pikkustadi-toolon tulli.3.tmx");
  REQUIRE(iter != proj->maps.end());

  // unknown entities are not suddenly found 

  iter = proj->maps.find("things that are not and should not be");
  REQUIRE(iter == proj->maps.end());

  // let's explore what the example map looks like
  Map& m = proj->maps.at("pikkustadi-toolon tulli.2.tmx");
  nlohmann::json js = m;
  REQUIRE(m.layers.size() == 2);
  REQUIRE(m.tileheight == m.tilewidth);
  REQUIRE(m.tilewidth == 32);

  for(auto& l: m.layers) {
    for(auto &c: l.chunks) {
      REQUIRE(l.width == c.width);
      REQUIRE(l.height == c.height);
      // TODO make sure which dimension is w, which h
      REQUIRE(c.width == c.tiles.size());
      REQUIRE(c.height == c.tiles.at(0).size());
    }

    REQUIRE(l.enc == CSV);
  }

  REQUIRE(m.tilesets.size() == 3);

  Tileset *random_tileset = m.tilesets.at(1);
  int tileset_w = random_tileset->width(),
    tileset_h = random_tileset->height();

  REQUIRE(tileset_w == 16);
  REQUIRE(tileset_h == 16);

  REQUIRE_THROWS(random_tileset->tileAt(tileset_w - (rand() % tileset_w),
					tileset_h + (rand() % tileset_h)));

  REQUIRE_NOTHROW(random_tileset->tileAt(tileset_w - (rand() % tileset_w),
					 tileset_h - (rand() % tileset_h)));


  for(auto& p: proj->maps) {
    Map& m = p.second;
    if(m.entry_script_found)
      REQUIRE_NOTHROW(proj->getScript(m.entry_script_id));
  }

  // printf("map json: %s\n", js.dump().c_str());
}

