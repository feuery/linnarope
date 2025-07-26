#include <project.h>
#include <result.h>
#include <app.h>
#include <test.h>
#include <vector>

#include <nlohmann/json.hpp>
#include <tmx_json.h>


Test tmx_test = Test([](std::vector<Result>& results){
  Project *proj = nullptr;
  proj = read_project("./test-resources/linnarope-export.game");
  // Project is loaded correctly with enough maps 
  ENSURE(proj);
  ENSURE(proj->maps.size() == 2);

  // Maps called the same as in the resource handler

  auto iter = proj->maps.find("pikkustadi-toolon tulli.2.tmx");
  ENSURE(iter != proj->maps.end());

  iter = proj->maps.find("pikkustadi-toolon tulli.3.tmx");
  ENSURE(iter != proj->maps.end());

  // unknown entities are not suddenly found 

  iter = proj->maps.find("things that are not and should not be");
  ENSURE(iter == proj->maps.end());

  // let's explore what the example map looks like
  Map& m = proj->maps.at("pikkustadi-toolon tulli.2.tmx");
  nlohmann::json js = m;
  ENSURE(m.layers.size() == 2);
  ENSURE(m.tileheight == m.tilewidth);
  ENSURE(m.tilewidth == 32);

  for(auto& l: m.layers) {
    for(auto &c: l.chunks) {
      ENSURE(l.width == c.width);
      ENSURE(l.height == c.height);
      // TODO make sure which dimension is w, which h
      ENSURE(c.width == c.tiles.size());
      ENSURE(c.height == c.tiles.at(0).size());
    }

    ENSURE(l.enc == CSV);
  }

  ENSURE(m.tilesets.size() == 3);

  Tileset *random_tileset = m.tilesets.at(1);
  int tileset_w = random_tileset->width(),
    tileset_h = random_tileset->height();

  ENSURE(tileset_w == 16);
  ENSURE(tileset_h == 16);

  


  

  ENSURE_THROWS(random_tileset->tileAt(tileset_w - (rand() % tileset_w),
				       tileset_h + (rand() % tileset_h)), std::out_of_range);

  ENSURE_NOTHROW(random_tileset->tileAt(tileset_w - (rand() % tileset_w),
					tileset_h - (rand() % tileset_h)));


  for(auto& p: proj->maps) {
    Map& m = p.second;
    if(m.entry_script_found)
      ENSURE_NOTHROW(proj->getScript(m.entry_script_id));
  }


  return true;
  
 },
  [](){ return true;},
  [](){ return true;},
  "tmx_test to be");

std::vector<Test> AutoTests::get_tests() {
  std::vector<Test> acc;
  acc.push_back(tmx_test);
  return acc;
}

void AutoTests::do_it() { RunAndReportTests(); }
  

void AutoTests::RunAndReportTests() {
  for(auto &test: get_tests()) {
    test.setup();

    test.run_test();
    test.report();
    // auto result = test.run_test();
    // printf("%s => %s\n", test.name().c_str(), result.result? "SUCCESS":"FAILURE");

    test.teardown();
  }
}
