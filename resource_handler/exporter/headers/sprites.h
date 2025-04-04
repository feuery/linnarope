#include <pqxx/pqxx>
#include <sqlite3.h>

bool export_sprites(pqxx::work &tx, sqlite3 *sql);

// contains palettes and stuff
bool export_lisp_sprites(pqxx::work &tx, sqlite3 *sql);
