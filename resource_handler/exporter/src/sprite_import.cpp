#include <sprite_import.h>

bool import_sprites(pqxx::work &tx, sqlite3 *db) {  
  sqlite3_stmt *stmt;
  std::string query_all = "SELECT internal_id, name, data FROM sprite";
  int rc = sqlite3_prepare_v2(db, query_all.c_str(), query_all.size(), &stmt, nullptr);

  if(rc != SQLITE_OK) {
    printf("import_sprites failed %s\n", sqlite3_errmsg(db));
    return false;
  }

  auto step_res = sqlite3_step(stmt);
  assert(step_res != SQLITE_ERROR);

  do {
    
    std::string psql_insert = R"(INSERT INTO sprite
(internal_id, name, data)
 VALUES
($1, $2, $3))";

    int internal_id = sqlite3_column_int(stmt, 0); 
    auto name_ = sqlite3_column_text(stmt, 1);
      
    std::string name (reinterpret_cast<const char*>(name_));

    const void * bin_data = sqlite3_column_blob(stmt, 2);
    size_t bin_size = sqlite3_column_bytes(stmt, 2);

    // binarystrings seem to be deprecated, but I can't understand how to insert blobs in a non deprecated way 
    pqxx::binarystring bin( bin_data, bin_size );

    
    pqxx::params p;
    p.append(internal_id);
    p.append(name);
    p.append(bin);
      
    tx.exec(psql_insert, p);

    step_res = sqlite3_step(stmt);
  } while (step_res == SQLITE_ROW);

  puts("Spritess done\n");

  sqlite3_finalize(stmt);
  return true;
}

bool import_palettes(pqxx::work &tx, sqlite3 *db)  {  
  sqlite3_stmt *stmt;
  std::string query_all = "SELECT id, name, color_array FROM palette";
  int rc = sqlite3_prepare_v2(db, query_all.c_str(), query_all.size(), &stmt, nullptr);

  if(rc != SQLITE_OK) {
    printf("import_sprites failed %s\n", sqlite3_errmsg(db));
    return false;
  }

  auto step_res = sqlite3_step(stmt);
  assert(step_res != SQLITE_ERROR);

  do {
    
    std::string psql_insert = R"(INSERT INTO palette
(id, name, color_array)
 VALUES
($1, $2, $3))";

    int id = sqlite3_column_int(stmt, 0); 
    auto name_ = sqlite3_column_text(stmt, 1),
      color_arr_ = sqlite3_column_text(stmt, 2);
      
    std::string name (reinterpret_cast<const char*>(name_)),
      color_arr (reinterpret_cast<const char*>(color_arr_));

    pqxx::params p;
    p.append(id);
    p.append(name);
    p.append(color_arr);
      
    tx.exec(psql_insert, p);

    step_res = sqlite3_step(stmt);
  } while (step_res == SQLITE_ROW);

  puts("Palettes done\n");

  sqlite3_finalize(stmt);
  return true;
}
  
bool import_lisp_sprites(pqxx::work &tx, sqlite3 *db) {
  if (! (import_palettes(tx, db))){
    tx.abort();
    return false;
  }
  
  sqlite3_stmt *stmt;
  std::string query_all = "SELECT id, name, w, h, palette_id, pixels FROM lisp_sprite";
  int rc = sqlite3_prepare_v2(db, query_all.c_str(), query_all.size(), &stmt, nullptr);

  if(rc != SQLITE_OK) {
    printf("import_sprites failed %s\n", sqlite3_errmsg(db));
    return false;
  }

  auto step_res = sqlite3_step(stmt);
  assert(step_res != SQLITE_ERROR);

  do {
    
    std::string psql_insert = R"(INSERT INTO lisp_sprite
(id, name, w, h, palette_id, pixels)
 VALUES
($1, $2, $3, $4, $5, $6))";

    auto name_ = sqlite3_column_text(stmt, 1);

    int id = sqlite3_column_int(stmt, 0),
      w = sqlite3_column_int(stmt, 2),
      h = sqlite3_column_int(stmt, 3),
      palette_id = sqlite3_column_int(stmt, 4);

    auto pixels_ = sqlite3_column_text(stmt, 5);

    std::string name (reinterpret_cast<const char*>(name_)),
      pixels (reinterpret_cast<const char*>(pixels_));
    
    pqxx::params p;
    p.append(id);
    p.append(name);
    p.append(w);
    p.append(h);
    p.append(palette_id);
    p.append(pixels);
      
    tx.exec(psql_insert, p);

    step_res = sqlite3_step(stmt);
  } while (step_res == SQLITE_ROW);

  puts("sprites done\n");

  sqlite3_finalize(stmt);
  
  return true;
}
