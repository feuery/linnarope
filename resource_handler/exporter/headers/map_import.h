#pragma once

#include <pqxx/pqxx>
#include <sqlite3.h>

// including warp_connections 
bool import_maps(pqxx::work &tx, sqlite3 *sql);
