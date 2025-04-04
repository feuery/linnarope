#pragma once

#include <pqxx/pqxx>
#include <sqlite3.h>

// including warp_connections 
bool export_maps(pqxx::work &tx, sqlite3 *sql);
