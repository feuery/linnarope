#pragma once

#include <pqxx/pqxx>
#include <sqlite3.h>

bool export_scripts(pqxx::work &tx, sqlite3 *db);
bool import_scripts(pqxx::work &tx, sqlite3 *db);
