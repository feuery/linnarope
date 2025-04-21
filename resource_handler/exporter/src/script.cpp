#include "map.h"
#include <script.h>
#include <string>

bool export_scripts(pqxx::work &tx, sqlite3 *db) {
  for(auto [id, name, script]: tx.query<int, std::string, std::string>("SELECT id, name, script FROM script")) {
    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO script (id, name, script) VALUES (?, ?, ?)";
    int result = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.size(), &stmt, nullptr);

    if(result != SQLITE_OK) {
      printf("export_scripts failed %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_int(stmt, 1, id);
    sqlite3_bind_text(stmt, 2, name.c_str(), name.size(), SQLITE_STATIC);
    sqlite3_bind_text(stmt, 3, script.c_str(), script.size(), SQLITE_STATIC);

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("export_scripts failed (2) %s\n", sqlite3_errmsg(db));
      return false;
    }
    sqlite3_finalize(stmt);
  }

  return true;
}

bool import_scripts(pqxx::work& t, sqlite3* db) {
  sqlite3_stmt *stmt;
  std::string q_all = "SELECT id, name, script FROM script";
  int result = sqlite3_prepare_v2(db, q_all.c_str(), q_all.size(), &stmt, nullptr);
  if(result != SQLITE_OK) {
    printf("Import_scripts failed %s\n", sqlite3_errmsg(db));
    return false;
  }

  int res2;
  while((res2 = sqlite3_step(stmt))  == SQLITE_ROW) {
    int id = sqlite3_column_int(stmt, 0);
    std::string name = (reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1))),
      script = (reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2)));

    pqxx::params p;
    p.append(id);
    p.append(name);
    p.append(script);

    t.exec("INSERT INTO script (id, name, script) VALUES ($1, $2, $3)", p);
  }

  sqlite3_finalize(stmt);
  return true;
}
