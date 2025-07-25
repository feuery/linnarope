#include <stdexcept>
#include <tmxreader.h> 
#include <project.h>
#include <nlohmann/json.hpp>
#include <tmx_json.h>

#include <gtest/gtest.h>


TEST(load_example_tmx, all) {
  Project *proj = nullptr;
  proj = read_project("./test-resources/linnarope-export.game");
  // Project is loaded correctly with enough maps 
  EXPECT_TRUE(proj);
  EXPECT_TRUE(proj->maps.size() == 2);

  // Maps called the same as in the resource handler

  auto iter = proj->maps.find("pikkustadi-toolon tulli.2.tmx");
  EXPECT_TRUE(iter != proj->maps.end());

  iter = proj->maps.find("pikkustadi-toolon tulli.3.tmx");
  EXPECT_TRUE(iter != proj->maps.end());

  // unknown entities are not suddenly found 

  iter = proj->maps.find("things that are not and should not be");
  EXPECT_TRUE(iter == proj->maps.end());

  // let's explore what the example map looks like
  Map& m = proj->maps.at("pikkustadi-toolon tulli.2.tmx");
  nlohmann::json js = m;
  EXPECT_TRUE(m.layers.size() == 2);
  EXPECT_TRUE(m.tileheight == m.tilewidth);
  EXPECT_TRUE(m.tilewidth == 32);

  for(auto& l: m.layers) {
    for(auto &c: l.chunks) {
      EXPECT_TRUE(l.width == c.width);
      EXPECT_TRUE(l.height == c.height);
      // TODO make sure which dimension is w, which h
      EXPECT_TRUE(c.width == c.tiles.size());
      EXPECT_TRUE(c.height == c.tiles.at(0).size());
    }

    EXPECT_TRUE(l.enc == CSV);
  }

  EXPECT_TRUE(m.tilesets.size() == 3);

  Tileset *random_tileset = m.tilesets.at(1);
  int tileset_w = random_tileset->width(),
    tileset_h = random_tileset->height();

  EXPECT_TRUE(tileset_w == 16);
  EXPECT_TRUE(tileset_h == 16);

  EXPECT_THROW(random_tileset->tileAt(tileset_w - (rand() % tileset_w),
				      tileset_h + (rand() % tileset_h)), std::out_of_range);

  EXPECT_NO_THROW(random_tileset->tileAt(tileset_w - (rand() % tileset_w),
					 tileset_h - (rand() % tileset_h)));


  for(auto& p: proj->maps) {
    Map& m = p.second;
    if(m.entry_script_found)
      EXPECT_NO_THROW(proj->getScript(m.entry_script_id));
  }

  // printf("map json: %s\n", js.dump().c_str());
}

