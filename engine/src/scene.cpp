#include "SDL_keycode.h"
#include "SDL_render.h"
#include "SDL_stdinc.h"
#include "SDL_surface.h"
#include "SDL_ttf.h"
#include <cmath>
#include <ecl/ecl.h>
#include <handle.h>
#include <string>
#include <tmxreader.h>
#include <swank.h>
#include <scene.h>
#include <engine_api.h>
#include <tmx_private.h>
#include <project.h>
#include <unordered_map>
#include <ropetimer.h>

// these two are moved from main.o 
Scene *current_scene = nullptr;
SDL_Renderer *current_renderer = nullptr;

Scene::Scene(Project *p, SDL_Renderer *r): proj(p), 
					   current_startup(ECL_NIL),
					   current_update(ECL_NIL),
					   current_teardown(ECL_NIL),
					   renderer(r),
					   current_color({255, 255, 255, 255})
{ }

void Scene::changeMap(Map *m){

  if(current_teardown != ECL_NIL) {
    cl_funcall(1, current_teardown);
  }
  
  current_startup = ECL_NIL;
  current_update = ECL_NIL;
  current_teardown = ECL_NIL;
  
  eval_entry_script(m);
  current_map = m;

  if (current_startup != ECL_NIL) {
    cl_funcall(1, current_startup);
  }
}

void Scene::update() {  
  if(this->current_update != ECL_NIL) {
    int handle = toHandle(current_map);
    cl_object handle_ = ecl_make_int(handle);
    cl_funcall(2, current_update, handle_);
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
    r = &proj->getLisp_Sprite(resourcename.c_str());
  }
  else if (type == "Palette") {
    r = &proj->getPalette(resourcename.c_str());
  }
  else if (type == "Sprite") {
    r = &proj->sprites.at(resourcename);
  } else printf("Unknown resource type %s\n", type.c_str());

  assert(r);

  return toHandle(r);
}

Resource* Scene::handle_to_resource(int hndl) {
  return fromHandle(hndl);
}

SDL_PixelFormat* Scene::currentFormat() {
  assert(current_map);
  return current_map->rendered_map->format;
}

void Scene::setKeydown(SDL_Keycode cde)
{
  keystate[cde] = true;
}

void Scene::setKeyup(SDL_Keycode cde)
{
  keystate[cde] = false;
}

static std::unordered_map<std::string, SDL_Keycode> keycodes = {
    {"SDLK_UNKNOWN", SDLK_UNKNOWN},

    {"SDLK_RETURN", SDLK_RETURN},
    {"SDLK_ESCAPE", SDLK_ESCAPE},
    {"SDLK_BACKSPACE", SDLK_BACKSPACE},
    {"SDLK_TAB", SDLK_TAB},
    {"SDLK_SPACE", SDLK_SPACE},
    {"SDLK_EXCLAIM", SDLK_EXCLAIM},
    {"SDLK_QUOTEDBL", SDLK_QUOTEDBL},
    {"SDLK_HASH", SDLK_HASH},
    {"SDLK_PERCENT", SDLK_PERCENT},
    {"SDLK_DOLLAR", SDLK_DOLLAR},
    {"SDLK_AMPERSAND", SDLK_AMPERSAND},
    {"SDLK_QUOTE", SDLK_QUOTE},
    {"SDLK_LEFTPAREN", SDLK_LEFTPAREN},
    {"SDLK_RIGHTPAREN", SDLK_RIGHTPAREN},
    {"SDLK_ASTERISK", SDLK_ASTERISK},
    {"SDLK_PLUS", SDLK_PLUS},
    {"SDLK_COMMA", SDLK_COMMA},
    {"SDLK_MINUS", SDLK_MINUS},
    {"SDLK_PERIOD", SDLK_PERIOD},
    {"SDLK_SLASH", SDLK_SLASH},
    {"SDLK_0", SDLK_0},
    {"SDLK_1", SDLK_1},
    {"SDLK_2", SDLK_2},
    {"SDLK_3", SDLK_3},
    {"SDLK_4", SDLK_4},
    {"SDLK_5", SDLK_5},
    {"SDLK_6", SDLK_6},
    {"SDLK_7", SDLK_7},
    {"SDLK_8", SDLK_8},
    {"SDLK_9", SDLK_9},
    {"SDLK_COLON", SDLK_COLON},
    {"SDLK_SEMICOLON", SDLK_SEMICOLON},
    {"SDLK_LESS", SDLK_LESS},
    {"SDLK_EQUALS", SDLK_EQUALS},
    {"SDLK_GREATER", SDLK_GREATER},
    {"SDLK_QUESTION", SDLK_QUESTION},
    {"SDLK_AT", SDLK_AT},

    /*
       Skip uppercase letters
     */

    {"SDLK_LEFTBRACKET", SDLK_LEFTBRACKET},
    {"SDLK_BACKSLASH", SDLK_BACKSLASH},
    {"SDLK_RIGHTBRACKET", SDLK_RIGHTBRACKET},
    {"SDLK_CARET", SDLK_CARET},
    {"SDLK_UNDERSCORE", SDLK_UNDERSCORE},
    {"SDLK_BACKQUOTE", SDLK_BACKQUOTE},
    {"SDLK_a", SDLK_a},
    {"SDLK_b", SDLK_b},
    {"SDLK_c", SDLK_c},
    {"SDLK_d", SDLK_d},
    {"SDLK_e", SDLK_e},
    {"SDLK_f", SDLK_f},
    {"SDLK_g", SDLK_g},
    {"SDLK_h", SDLK_h},
    {"SDLK_i", SDLK_i},
    {"SDLK_j", SDLK_j},
    {"SDLK_k", SDLK_k},
    {"SDLK_l", SDLK_l},
    {"SDLK_m", SDLK_m},
    {"SDLK_n", SDLK_n},
    {"SDLK_o", SDLK_o},
    {"SDLK_p", SDLK_p},
    {"SDLK_q", SDLK_q},
    {"SDLK_r", SDLK_r},
    {"SDLK_s", SDLK_s},
    {"SDLK_t", SDLK_t},
    {"SDLK_u", SDLK_u},
    {"SDLK_v", SDLK_v},
    {"SDLK_w", SDLK_w},
    {"SDLK_x", SDLK_x},
    {"SDLK_y", SDLK_y},
    {"SDLK_z", SDLK_z},

    {"SDLK_CAPSLOCK", SDLK_CAPSLOCK},

    {"SDLK_F1", SDLK_F1},
    {"SDLK_F2", SDLK_F2},
    {"SDLK_F3", SDLK_F3},
    {"SDLK_F4", SDLK_F4},
    {"SDLK_F5", SDLK_F5},
    {"SDLK_F6", SDLK_F6},
    {"SDLK_F7", SDLK_F7},
    {"SDLK_F8", SDLK_F8},
    {"SDLK_F9", SDLK_F9},
    {"SDLK_F10", SDLK_F10},
    {"SDLK_F11", SDLK_F11},
    {"SDLK_F12", SDLK_F12},

    {"SDLK_PRINTSCREEN", SDLK_PRINTSCREEN},
    {"SDLK_SCROLLLOCK", SDLK_SCROLLLOCK},
    {"SDLK_PAUSE", SDLK_PAUSE},
    {"SDLK_INSERT", SDLK_INSERT},
    {"SDLK_HOME", SDLK_HOME},
    {"SDLK_PAGEUP", SDLK_PAGEUP},
    {"SDLK_DELETE", SDLK_DELETE},
    {"SDLK_END", SDLK_END},
    {"SDLK_PAGEDOWN", SDLK_PAGEDOWN},
    {"SDLK_RIGHT", SDLK_RIGHT},
    {"SDLK_LEFT", SDLK_LEFT},
    {"SDLK_DOWN", SDLK_DOWN},
    {"SDLK_UP", SDLK_UP},

    {"SDLK_NUMLOCKCLEAR", SDLK_NUMLOCKCLEAR},
    {"SDLK_KP_DIVIDE", SDLK_KP_DIVIDE},
    {"SDLK_KP_MULTIPLY", SDLK_KP_MULTIPLY},
    {"SDLK_KP_MINUS", SDLK_KP_MINUS},
    {"SDLK_KP_PLUS", SDLK_KP_PLUS},
    {"SDLK_KP_ENTER", SDLK_KP_ENTER},
    {"SDLK_KP_1", SDLK_KP_1},
    {"SDLK_KP_2", SDLK_KP_2},
    {"SDLK_KP_3", SDLK_KP_3},
    {"SDLK_KP_4", SDLK_KP_4},
    {"SDLK_KP_5", SDLK_KP_5},
    {"SDLK_KP_6", SDLK_KP_6},
    {"SDLK_KP_7", SDLK_KP_7},
    {"SDLK_KP_8", SDLK_KP_8},
    {"SDLK_KP_9", SDLK_KP_9},
    {"SDLK_KP_0", SDLK_KP_0},
    {"SDLK_KP_PERIOD", SDLK_KP_PERIOD},

    {"SDLK_APPLICATION", SDLK_APPLICATION},
    {"SDLK_POWER", SDLK_POWER},
    {"SDLK_KP_EQUALS", SDLK_KP_EQUALS},
    {"SDLK_F13", SDLK_F13},
    {"SDLK_F14", SDLK_F14},
    {"SDLK_F15", SDLK_F15},
    {"SDLK_F16", SDLK_F16},
    {"SDLK_F17", SDLK_F17},
    {"SDLK_F18", SDLK_F18},
    {"SDLK_F19", SDLK_F19},
    {"SDLK_F20", SDLK_F20},
    {"SDLK_F21", SDLK_F21},
    {"SDLK_F22", SDLK_F22},
    {"SDLK_F23", SDLK_F23},
    {"SDLK_F24", SDLK_F24},
    {"SDLK_EXECUTE", SDLK_EXECUTE},
    {"SDLK_HELP", SDLK_HELP},
    {"SDLK_MENU", SDLK_MENU},
    {"SDLK_SELECT", SDLK_SELECT},
    {"SDLK_STOP", SDLK_STOP},
    {"SDLK_AGAIN", SDLK_AGAIN},
    {"SDLK_UNDO", SDLK_UNDO},
    {"SDLK_CUT", SDLK_CUT},
    {"SDLK_COPY", SDLK_COPY},
    {"SDLK_PASTE", SDLK_PASTE},
    {"SDLK_FIND", SDLK_FIND},
    {"SDLK_MUTE", SDLK_MUTE},
    {"SDLK_VOLUMEUP", SDLK_VOLUMEUP},
    {"SDLK_VOLUMEDOWN", SDLK_VOLUMEDOWN},
    {"SDLK_KP_COMMA", SDLK_KP_COMMA},
    {"SDLK_KP_EQUALSAS400", SDLK_KP_EQUALSAS400},
    {"SDLK_ALTERASE", SDLK_ALTERASE},
    {"SDLK_SYSREQ", SDLK_SYSREQ},
    {"SDLK_CANCEL", SDLK_CANCEL},
    {"SDLK_CLEAR", SDLK_CLEAR},
    {"SDLK_PRIOR", SDLK_PRIOR},
    {"SDLK_RETURN2", SDLK_RETURN2},
    {"SDLK_SEPARATOR", SDLK_SEPARATOR},
    {"SDLK_OUT", SDLK_OUT},
    {"SDLK_OPER", SDLK_OPER},
    {"SDLK_CLEARAGAIN", SDLK_CLEARAGAIN},
    {"SDLK_CRSEL", SDLK_CRSEL},
    {"SDLK_EXSEL", SDLK_EXSEL},

    {"SDLK_KP_00", SDLK_KP_00},
    {"SDLK_KP_000", SDLK_KP_000},
    {"SDLK_THOUSANDSSEPARATOR", SDLK_THOUSANDSSEPARATOR},
    {"SDLK_DECIMALSEPARATOR", SDLK_DECIMALSEPARATOR},
    {"SDLK_CURRENCYUNIT", SDLK_CURRENCYUNIT},
    {"SDLK_CURRENCYSUBUNIT", SDLK_CURRENCYSUBUNIT},
    {"SDLK_KP_LEFTPAREN", SDLK_KP_LEFTPAREN},
    {"SDLK_KP_RIGHTPAREN", SDLK_KP_RIGHTPAREN},
    {"SDLK_KP_LEFTBRACE", SDLK_KP_LEFTBRACE},
    {"SDLK_KP_RIGHTBRACE", SDLK_KP_RIGHTBRACE},
    {"SDLK_KP_TAB", SDLK_KP_TAB},
    {"SDLK_KP_BACKSPACE", SDLK_KP_BACKSPACE},
    {"SDLK_KP_A", SDLK_KP_A},
    {"SDLK_KP_B", SDLK_KP_B},
    {"SDLK_KP_C", SDLK_KP_C},
    {"SDLK_KP_D", SDLK_KP_D},
    {"SDLK_KP_E", SDLK_KP_E},
    {"SDLK_KP_F", SDLK_KP_F},
    {"SDLK_KP_XOR", SDLK_KP_XOR},
    {"SDLK_KP_POWER", SDLK_KP_POWER},
    {"SDLK_KP_PERCENT", SDLK_KP_PERCENT},
    {"SDLK_KP_LESS", SDLK_KP_LESS},
    {"SDLK_KP_GREATER", SDLK_KP_GREATER},
    {"SDLK_KP_AMPERSAND", SDLK_KP_AMPERSAND},
    {"SDLK_KP_DBLAMPERSAND", SDLK_KP_DBLAMPERSAND},
    {"SDLK_KP_VERTICALBAR", SDLK_KP_VERTICALBAR},
    {"SDLK_KP_DBLVERTICALBAR", SDLK_KP_DBLVERTICALBAR},
    {"SDLK_KP_COLON", SDLK_KP_COLON},
    {"SDLK_KP_HASH", SDLK_KP_HASH},
    {"SDLK_KP_SPACE", SDLK_KP_SPACE},
    {"SDLK_KP_AT", SDLK_KP_AT},
    {"SDLK_KP_EXCLAM", SDLK_KP_EXCLAM},
    {"SDLK_KP_MEMSTORE", SDLK_KP_MEMSTORE},
    {"SDLK_KP_MEMRECALL", SDLK_KP_MEMRECALL},
    {"SDLK_KP_MEMCLEAR", SDLK_KP_MEMCLEAR},
    {"SDLK_KP_MEMADD", SDLK_KP_MEMADD},
    {"SDLK_KP_MEMSUBTRACT", SDLK_KP_MEMSUBTRACT},
    {"SDLK_KP_MEMMULTIPLY", SDLK_KP_MEMMULTIPLY},
    {"SDLK_KP_MEMDIVIDE", SDLK_KP_MEMDIVIDE},
    {"SDLK_KP_PLUSMINUS", SDLK_KP_PLUSMINUS},
    {"SDLK_KP_CLEAR", SDLK_KP_CLEAR},
    {"SDLK_KP_CLEARENTRY", SDLK_KP_CLEARENTRY},
    {"SDLK_KP_BINARY", SDLK_KP_BINARY},
    {"SDLK_KP_OCTAL", SDLK_KP_OCTAL},
    {"SDLK_KP_DECIMAL", SDLK_KP_DECIMAL},
    {"SDLK_KP_HEXADECIMAL", SDLK_KP_HEXADECIMAL},

    {"SDLK_LCTRL", SDLK_LCTRL},
    {"SDLK_LSHIFT", SDLK_LSHIFT},
    {"SDLK_LALT", SDLK_LALT},
    {"SDLK_LGUI", SDLK_LGUI},
    {"SDLK_RCTRL", SDLK_RCTRL},
    {"SDLK_RSHIFT", SDLK_RSHIFT},
    {"SDLK_RALT", SDLK_RALT},
    {"SDLK_RGUI", SDLK_RGUI},

    {"SDLK_MODE", SDLK_MODE},

    {"SDLK_AUDIONEXT", SDLK_AUDIONEXT},
    {"SDLK_AUDIOPREV", SDLK_AUDIOPREV},
    {"SDLK_AUDIOSTOP", SDLK_AUDIOSTOP},
    {"SDLK_AUDIOPLAY", SDLK_AUDIOPLAY},
    {"SDLK_AUDIOMUTE", SDLK_AUDIOMUTE},
    {"SDLK_MEDIASELECT", SDLK_MEDIASELECT},
    {"SDLK_WWW", SDLK_WWW},
    {"SDLK_MAIL", SDLK_MAIL},
    {"SDLK_CALCULATOR", SDLK_CALCULATOR},
    {"SDLK_COMPUTER", SDLK_COMPUTER},
    {"SDLK_AC_SEARCH", SDLK_AC_SEARCH},
    {"SDLK_AC_HOME", SDLK_AC_HOME},
    {"SDLK_AC_BACK", SDLK_AC_BACK},
    {"SDLK_AC_FORWARD", SDLK_AC_FORWARD},
    {"SDLK_AC_STOP", SDLK_AC_STOP},
    {"SDLK_AC_REFRESH", SDLK_AC_REFRESH},
    {"SDLK_AC_BOOKMARKS", SDLK_AC_BOOKMARKS},

    {"SDLK_BRIGHTNESSDOWN", SDLK_BRIGHTNESSDOWN},
    {"SDLK_BRIGHTNESSUP", SDLK_BRIGHTNESSUP},
    {"SDLK_DISPLAYSWITCH", SDLK_DISPLAYSWITCH},
    {"SDLK_KBDILLUMTOGGLE", SDLK_KBDILLUMTOGGLE},
    {"SDLK_KBDILLUMDOWN", SDLK_KBDILLUMDOWN},
    {"SDLK_KBDILLUMUP", SDLK_KBDILLUMUP},
    {"SDLK_EJECT", SDLK_EJECT},
    {"SDLK_SLEEP", SDLK_SLEEP},
    {"SDLK_APP1", SDLK_APP1},
    {"SDLK_APP2", SDLK_APP2},

    {"SDLK_AUDIOREWIND", SDLK_AUDIOREWIND},
    {"SDLK_AUDIOFASTFORWARD", SDLK_AUDIOFASTFORWARD},

    {"SDLK_SOFTLEFT", SDLK_SOFTLEFT},
    {"SDLK_SOFTRIGHT", SDLK_SOFTRIGHT},
    {"SDLK_CALL", SDLK_CALL},
    {"SDLK_ENDCALL", SDLK_ENDCALL}};

std::unordered_map<SDL_Keycode, long> key_last_queried;
  

SDL_Keycode translate(std::string &str) {
  return keycodes.at(str);
}

bool Scene::is_keydown(std::string &keystr) {
  auto keycode = translate(keystr);
  
  long timer = mstimer();
  bool result = false;

  long last_queried = key_last_queried[keycode];

  if ((timer - last_queried) < 600l) {
    // printf("Less than 600ms from %d the time key was last down\n", last_queried);
    // printf("Timers is %ld\n", timer);
  }
  else {
    result = keystate[keycode];
  }

  if(result) { key_last_queried[keycode] = timer;}
  return result;
}

// returns radians 
double angl (int x1, int y1, int x2, int y2) {
  return atan((x2 - x1) / (y2 - y1));
}

void Scene::line(int x1, int y1, int x2, int y2, int thickness) {
  if(thickness == 1) {
    SDL_RenderDrawLine(renderer, x1, y1, x2, y2);
    return;
  }

  double angle_rads = angl(x1, y1, x2, y2);
  // I think PI/2 RAD == 90°
  angle_rads += M_PI_2;

  for(double i = 0.0; i < thickness; i++) {
    double xx1 = x1 + (i * cos(angle_rads)),
      yy1 = y1 + (i * sin(angle_rads)),
      xx2 = x2 + (i * cos(angle_rads)),
      yy2 = y2 + (i * sin(angle_rads));
    
    SDL_RenderDrawLine(renderer, xx1, yy1, xx2, yy2);
  }  
}

void Scene::setColor(Uint8 r, Uint8 g, Uint8 b) {
  SDL_SetRenderDrawColor(renderer, r, g, b, 255);
  current_color = {r, g, b, 255};
}


void Scene::loadFont(const char* fontPath) {
  font = TTF_OpenFont(fontPath, 18);

  if(font) return;

  printf("Error loading font %s: %s\n", fontPath, TTF_GetError());
  
  assert(font);
}

void Scene::drawText(std::string& txt, int x, int y) {
  SDL_Surface *txt_srfc = TTF_RenderUTF8_Solid(font, txt.c_str(), current_color);
  assert(txt_srfc);

  SDL_Texture *txt_txt =  SDL_CreateTextureFromSurface(renderer, txt_srfc);
  SDL_Rect loc = {x, y, txt_srfc->w, txt_srfc->h};
  SDL_RenderCopy(renderer, txt_txt, nullptr, &loc);

  SDL_DestroyTexture(txt_txt);
  SDL_FreeSurface(txt_srfc);
}
		     
