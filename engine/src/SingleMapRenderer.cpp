#include <libgen.h>
#include <lib_fixup.h>
#include <app.h>
#include <tmxreader.h>
#include <SDL.h>
#include <SDL_image.h>
#include <tmx_private.h>

SingleMapRenderer::SingleMapRenderer(std::string map_file, std::string png_output_file): map_file(map_file), png_output_file(png_output_file) {}

char* unconst_str(const char *string, int bytes) {
  char *c = new char[bytes];
  for(int i = 0; i < bytes; i++) { c[i] = string[i]; }
  return c;
}

void SingleMapRenderer::do_it() {
  printf("Rendering a single map into %s\n", png_output_file.c_str());
  SDL_Window *w = createWindow(true);
  SDL_Renderer *r = createRenderer(w);

  std::string map_contents = get_file_contents(map_file.c_str());
    
  Map m = tmx_to_map(map_contents.c_str());
  char *filenme = unconst_str(map_file.c_str(), map_file.size());

  const char *basepath = dirname(filenme);

  delete [] filenme;
    
  m.loadTilesets(basepath);
  render_map(&m, r);

  IMG_SavePNG(m.rendered_map, png_output_file.c_str());

  printf("Saved %s as %s\n", map_file.c_str(), png_output_file.c_str());
}
