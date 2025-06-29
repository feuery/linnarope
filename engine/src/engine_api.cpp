#include <cassert>
#include <ropetimer.h>
#include <ecl/ecl.h>
#include <tmx_private.h>
#include <tmxreader.h>
#include <engine_api.h>
#include <finrope.h>
#include <cstdio>
#include <swank.h>
#include <scene.h>
#include <stdexcept>

cl_object render(cl_object handle, cl_object x_, cl_object y_);

void register_callbacks() {
  ecl_call("(defpackage engine (:use :cl) (:export :keydown? :get-sprite :setup-scene :get-resource :render :change-map :mstimer :draw-line :set-color :draw-text))");
  DEFUN("engine:setup-scene", setup_scene, 3);
  DEFUN("engine:get-resource", get_resource, 2);
  DEFUN("engine:render", render, 3);
  DEFUN("engine:change-map", change_map, 1);
  DEFUN("engine:keydown?", is_keydown, 1);
  DEFUN("engine:mstimer", timer, 0);
  DEFUN("engine:draw-line", draw_line, 5);
  DEFUN("engine:set-color", set_color, 3);
  DEFUN("engine:draw-text", draw_text, 3);
}

cl_object draw_text(cl_object txt, cl_object x, cl_object y) {
  int xx = ecl_to_int(x),
    yy = ecl_to_int(y);
  std::string txtt = ecl_string_to_string(txt);

  assert(current_scene);
  current_scene->drawText(txtt, xx, yy);
  return ECL_T;
}

cl_object set_color(cl_object r, cl_object g, cl_object b) {
  int rr = ecl_to_int(r),
    gg = ecl_to_int(g),
    bb = ecl_to_int(b);

  assert(current_scene);
  current_scene->setColor(rr,gg,bb);

  return ECL_T;
}

cl_object draw_line(cl_object x1, cl_object y1, cl_object x2, cl_object y2, cl_object thickness) {
  assert(current_scene);

  int xx1 = ecl_to_int(x1),
    yy1 = ecl_to_int(y1),
    xx2 = ecl_to_int(x2),
    yy2 = ecl_to_int(y2),
    thick = ecl_to_int(thickness);

  current_scene->line(xx1, yy1, xx2, yy2, thick);
  
  return ECL_T;
}

cl_object setup_scene(cl_object startup, cl_object update, cl_object teardown) {
  if(!current_scene) {
    puts("Current scene is not set up :(\n");
    return ECL_NIL;
  }
  current_scene->register_callbacks(startup, update, teardown);
  
  puts("Registered a new scene");
  return ecl_call("\"Registered a new scene\"");
}

cl_object get_resource(cl_object _typename, cl_object resourcename) {
  assert(current_scene);

  std::string typename_ = ecl_string_to_string(_typename),
    resourcename_ = ecl_string_to_string(resourcename);  

  try {
    int handle = current_scene->resource_to_handle(typename_.c_str(), resourcename_.c_str());

    return ecl_make_int(handle);
  } catch (std::out_of_range _) {
    return ECL_NIL;
  }
}

cl_object render(cl_object handle, cl_object x_, cl_object y_) {
  assert(current_scene);

  int handle_ = ecl_to_int(handle),
    x = ecl_to_int(x_),
    y = ecl_to_int(y_);
  Resource *resource = current_scene->handle_to_resource(handle_);  

  resource->render_to_screen(x, y);
  
  return ECL_NIL;
}

cl_object change_map(cl_object map_handle) {
  assert(current_scene);
  int handle_ = ecl_to_int(map_handle);
  Resource *map = current_scene->handle_to_resource(handle_);

  assert(std::string("Map") == map->get_typename());
  Map *m = static_cast<Map*>(map);

  current_scene->changeMap(m);
  
  return ECL_NIL;
}

cl_object is_keydown(cl_object keystr) {
  std::string std_keystr = ecl_string_to_string(keystr);
  assert(current_scene);

  bool is_keydown = current_scene->is_keydown(std_keystr);
  
  return is_keydown ? ECL_T: ECL_NIL;
}

cl_object timer() {
  return ecl_make_int(mstimer());
}
