#pragma once

#include <pqxx/pqxx>
#include <sqlite3.h>

bool import_sprites(pqxx::work &tx, sqlite3 *db);
bool import_lisp_sprites(pqxx::work &tx, sqlite3 *db);
