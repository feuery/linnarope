#include <handle.h>
#include <stdexcept>
#include <string>
#include <tmxreader.h>
#include <swank.h>
#include <scene.h>
#include <engine_api.h>
#include <tmx_private.h>
#include <project.h>

Scene::Scene(Project *p): proj(p), 
			  current_startup(ECL_NIL),
			  current_update(ECL_NIL),
			  current_teardown(ECL_NIL)
{ }

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

int Scene::resource_to_handle(const char *typename_, const char *resourcename_) {
  assert(proj);

  Resource *r = nullptr;
  std::string type(typename_);
  std::string resourcename(resourcename_);
  
  if(type == "Map") {
    r = &proj->maps.at(resourcename);
  }
  else if(type == "Script") {
    r = &proj->getScript(resourcename.c_str());
  }
  else if (type == "Lisp sprite") {
    r = &proj->lisp_sprites.at(resourcename);
  }
  else if (type == "Palette") {
    r = &proj->palettes.at(resourcename);
  }
  else if (type == "Sprite") {
    r = &proj->sprites.at(resourcename);
  }

  assert(r);

  return toHandle(r);
}

Resource* Scene::handle_to_resource(int hndl) {
  return fromHandle(hndl);
}
