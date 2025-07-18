#include <sprites.h>
#include <string>

bool export_sprites(pqxx::work &tx, sqlite3 *db) {
  int counter = 0;
  for(auto [internal_id, name, data]:
	tx.query<int, std::string, pqxx::bytes>("SELECT internal_id, name, data FROM sprite")) {
    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO sprite(internal_id, name, data) VALUES (?, ?,?)";

    int rc = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.length() , &stmt, nullptr);
    if(rc != SQLITE_OK) {
      printf("fail 5: %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_int(stmt, 1, internal_id);
    sqlite3_bind_text(stmt, 2, name.c_str(), name.size(), SQLITE_STATIC);
    sqlite3_bind_blob(stmt, 3, data.data(), data.size(), SQLITE_STATIC);

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("fail 6: %s\n", sqlite3_errmsg(db));
      return false;
    }
    else counter++;
    
    sqlite3_finalize(stmt);
  }

  printf("Exported %d sprites\n", counter);

  return true;
}

bool export_palettes(pqxx::work &tx, sqlite3 *db) {
  int counter = 0;
  for(auto [id, name, color_array]:
	tx.query<int, std::string, std::string>("SELECT id, name, color_array FROM palette")) {
    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO palette(id, name, color_array) VALUES (?, ?, ?)";

    int rc = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.length() , &stmt, nullptr);
    if(rc != SQLITE_OK) {
      printf("fail 5: %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_int(stmt, 1, id);
    sqlite3_bind_text(stmt, 2, name.c_str(), name.size(), SQLITE_STATIC);
    sqlite3_bind_text(stmt, 3, color_array.c_str(), color_array.size(), SQLITE_STATIC);

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("fail 6: %s\n", sqlite3_errmsg(db));
      return false;
    }
    else counter++;
    
    sqlite3_finalize(stmt);
  }

  printf("Exported %d palettes\n", counter);

  return true;
}

bool export_lisp_sprites(pqxx::work &tx, sqlite3 *db) {

  if(!export_palettes(tx, db)) {
    puts("export palettes failed\n");
    return false;
  }
  
  int counter = 0;
  for(auto [id, name, w, h, palette_id, pixels]:
	tx.query<int, std::string, int, int, int, std::string>("SELECT id, name, w, h, palette_id, pixels FROM lisp_sprite")) {
    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO lisp_sprite(id, name, w, h, palette_id, pixels) VALUES (?, ?, ?, ?, ?, ?)";

    int rc = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.length() , &stmt, nullptr);
    if(rc != SQLITE_OK) {
      printf("fail 5: %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_int(stmt, 1, id);
    sqlite3_bind_text(stmt, 2, name.c_str(), name.size(), SQLITE_STATIC);
    sqlite3_bind_int(stmt, 3, w);
    sqlite3_bind_int(stmt, 4, h);
    sqlite3_bind_int(stmt, 5, palette_id);
    sqlite3_bind_text(stmt, 6, pixels.c_str(), pixels.size(), SQLITE_STATIC);

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("fail 6: %s\n", sqlite3_errmsg(db));
      return false;
    }
    else counter++;
    
    sqlite3_finalize(stmt);
  }

  printf("Exported %d sprites\n", counter);

  return true;
}
