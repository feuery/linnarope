#include <cassert>
#include <simulated_drawing.h>
#include <SDL.h>
#include <vector>

struct xywh_tuple {
  int x, y, w, h;
  SDL_Surface *srfc;
};

struct drawing_state {
  std::vector<xywh_tuple> drawn_objects;

  int smallest_x, smallest_y, highest_x, highest_y;
  int highest_x_width, highest_y_height;
};

int final_surface_w(drawing_state *ctx) {
  return ctx->highest_x + ctx->highest_x_width - ctx->smallest_x;
}

int final_surface_h (drawing_state* ctx) {
  return ctx->highest_y + ctx->highest_y_height - ctx->smallest_y;
}

void draw(drawing_state *ctx, int x, int y, int w, int h, SDL_Surface *srfc) {
  ctx->drawn_objects.push_back({x, y, w, h, srfc});

  printf("Drawing at %d, %d\n", x, y);

  if (x < ctx->smallest_x)
    ctx->smallest_x = x;
  else if ( x >= ctx->highest_x) {
    ctx->highest_x = x;
    ctx->highest_x_width = w;
  }

  if (y < ctx->smallest_y)
    ctx->smallest_y = y;
  else if (y >= ctx->highest_y ) {
    ctx->highest_y = y;
    ctx->highest_y_height = h;
  }
}


drawing_state* start_drawing() { return new drawing_state; }

SDL_Surface* stop_drawing(drawing_state *ctx)
{
  SDL_Surface *srfc = nullptr;
  if(!ctx->drawn_objects.empty()) {
    auto format = ctx->drawn_objects.at(0).srfc->format;
    int w = final_surface_w(ctx),
      h = final_surface_h(ctx);

    assert(w > 0);
    assert(h > 0);
    
    srfc = SDL_CreateRGBSurface(0,
				w,
				h,
				32,
				format->Rmask, format->Gmask, format->Bmask, format->Amask);
    
    for(xywh_tuple& obj: ctx->drawn_objects) {
      SDL_Rect rect = { obj.x - ctx->smallest_x,
			obj.y - ctx->smallest_y,

			obj.w, obj.h};
      
      auto blit_result = SDL_BlitSurface(obj.srfc, nullptr,
					 srfc, &rect);
      assert(blit_result == 0);
    }
  }
  else puts("ctx->drawn_objects is empty\n");

  delete ctx;
  return srfc;
}
