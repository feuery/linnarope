#include <project.h>
#include <tmxreader.h>
#include <app.h>
#include <sqlite3.h>
#include <cassert>
#include <SDL_image.h>

// throws dice and gets one map that is a src of a warp connection
std::string get_initial_tmx_path(const char *sqlite_path) {
  std::string result;
  sqlite3 *db;
  sqlite3_stmt *stmt;
  sqlite3_open(sqlite_path, &db);

  sqlite3_prepare(db, "SELECT tmx_path FROM Map WHERE ID IN (SELECT src_map FROM warp_connection) LIMIT 1", -1, &stmt, nullptr);

  if(sqlite3_step(stmt) == SQLITE_ROW) {
    auto txt = sqlite3_column_text(stmt, 0);
    assert(txt);
    result = std::string (reinterpret_cast<const char*>(txt));
  }
  else puts("Didn't find a suitable map");
  
  sqlite3_close(db);

  return result;
}

WholeMapRenderer::WholeMapRenderer(std::string sqlite_file, std::string png_output_file): sqlite_path(sqlite_file), png_output_file(png_output_file) {}

void WholeMapRenderer::do_it() {    
    // std::string initial_tmx_path = get_initial_tmx_path(sqlite_path.c_str());

    // if(initial_tmx_path == "") {
    //   puts("Didn't find a suitable map to start drawing from \n");
    //   return;
    // }

    // assert(initial_tmx_path != "");
    
  // printf("Making a huge map of %s and %s\n", sqlite_path.c_str(), initial_tmx_path.c_str());

    SDL_Window *w = createWindow(true);
    SDL_Renderer *r = createRenderer(w);

    Project *proj = read_project(sqlite_path.c_str());

    Map *initial_map = nullptr;

    for(auto& pair: proj->maps) {
      Map& m = pair.second;
      if(m.warpzones.empty()) continue;
      
      initial_map = &m;
      break;
    }

    if(!initial_map) {
      puts("Didn't find a suitable starting map with warpzones\n");
      return;
    }
    
    drawing_state *ctx = start_drawing();

    generate_drawing_context(proj, initial_map, ctx, r);

    SDL_Surface *final_map = stop_drawing(ctx);

    IMG_SavePNG(final_map, png_output_file.c_str());

    printf("Saved the whole map (%d x %d) to %s\n", final_map->w, final_map->h, png_output_file.c_str());
    SDL_FreeSurface(final_map);
}
