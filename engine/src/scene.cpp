#include "tmxreader.h"
#include <swank.h>
#include <scene.h>
#include <engine_api.h>

Scene::Scene(// Project *p
	     ) : // proj(p), 
		 current_startup(ECL_NIL), current_update(ECL_NIL), current_teardown(ECL_NIL) {}

void Scene::changeMap(Map *m) {}

void Scene::update() {
  
  if(this->current_update != ECL_NIL) {
    cl_funcall(1, current_update);
  }
}

void Scene::register_callbacks(cl_object startup, cl_object update, cl_object teardown) {
  this->current_startup = startup;
  this->current_update = update;
  this->current_teardown = teardown;
}
