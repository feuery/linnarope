#pragma once 
/**
 * This module provides an abstraction for drawing on an "infinite" surface. After starting a
 * drawing session, the first image will be "drawn" at (0,0) and the rest will grow the canvas
 * accordingly.
 *
 * After you're done drawing stuff, you can query the size of the resulting surface and
 * final coordinates of every image and draw them yourself :D 
 */

struct drawing_state;

#include <vector>
#include <tmxreader.h>

drawing_state* start_drawing();
void draw(drawing_state* ctx, int x, int y, int w, int h, SDL_Surface *srfc);

/**
 * deletes the ctx
 */
SDL_Surface* stop_drawing(drawing_state *ctx);
