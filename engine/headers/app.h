#pragma once

#include <string>
#include <SDL.h>

class App {
public:
  SDL_Window* createWindow(bool hidden = false);
  SDL_Renderer* createRenderer(SDL_Window *window);
  
  virtual void do_it() = 0;
  virtual ~App();
};

class Game: public App {
private :
  std::string sqlite_path;
  int argc;
  char **argv;
  
public:
  Game(int argc, char **argv, std::string sqlite_path);
  
  void do_it() override;
};

class WholeMapRenderer: public App {
private:
  std::string sqlite_path, png_output_file;

public:
  WholeMapRenderer(std::string sqlite_file, std::string png_output_file);
  
  void do_it() override;
};

class SingleMapRenderer: public App {
private:
  std::string map_file, png_output_file;

public:
  SingleMapRenderer(std::string map_file, std::string png_output_file);
    
  void do_it() override;
};

class ScriptExport: public App {
  std::string dst_directory, project_path;
  
public:

  ScriptExport(std::string project_path, std::string dst_dir);
  void do_it() override;
};

class ScriptImport: public App {
  std::string src_dir, project_path;
public:

  ScriptImport(std::string project_path, std::string src_dir);
  void do_it() override;
};
