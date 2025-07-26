#pragma once

#include <pqxx/pqxx>

void setup_test_db(pqxx::work &tx);
void setup_sprites(pqxx::work &tx);
