#pragma once

#include <tmx_private.h>

class Sprite: public Resource {
  const char* get_typename() override;
};

class Palette: public Resource {
public:
  const char* get_typename() override;  
};

class Lisp_sprite: public Resource {

public:
  const char* get_typename() override;

};
