#include <map_import.h>
#include <string>

#define text std::string

bool import_warp_connections(pqxx::work &tx, sqlite3 *db) {
  sqlite3_stmt *stmt;
  std::string query_all = "SELECT * FROM warp_connection";
  int rc = sqlite3_prepare_v2(db, query_all.c_str(), query_all.size(), &stmt, nullptr);

  if(rc != SQLITE_OK) {
    printf("import_warp_connections failed %s\n", sqlite3_errmsg(db));
    return false;
  }

  auto step_res = sqlite3_step(stmt);
  assert(step_res != SQLITE_ERROR);

  do {
    std::string psql_insert = "INSERT INTO warp_connection (internal_id, src_map, src_warpzone, dst_map, dst_warpzone) VALUES($1, $2, $3, $4, $5);";

    int internal_id = sqlite3_column_int(stmt, 0), 
      src_map = sqlite3_column_int(stmt, 1),
      src_warpzone = sqlite3_column_int(stmt, 2),
      dst_map = sqlite3_column_int(stmt, 3), 
      dst_warpzone = sqlite3_column_int(stmt, 4);
    
    pqxx::params p;
    p.append(internal_id);
    p.append(src_map);
    p.append(src_warpzone);
    p.append(dst_map);
    p.append(dst_warpzone);

    tx.exec(psql_insert, p);

    step_res = sqlite3_step(stmt);
  } while (step_res == SQLITE_ROW);

  sqlite3_finalize(stmt);
  puts("import_warp_connections done\n");
  return true;
}

bool import_objects(pqxx::work &tx, sqlite3 *db) {  
  sqlite3_stmt *stmt;
  std::string query_all = "SELECT internal_id, id, name, x, y, width, height, group_id, warp_zone FROM object";
  int rc = sqlite3_prepare_v2(db, query_all.c_str(), query_all.size(), &stmt, nullptr);

  if(rc != SQLITE_OK) {
    printf("import_objects failed %s\n", sqlite3_errmsg(db));
    return false;
  }

  auto step_res = sqlite3_step(stmt);
  assert(step_res != SQLITE_ERROR);

  do {
    std::string psql_insert = "INSERT INTO object (internal_id, id, name, x, y, width, height, group_id, warp_zone) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)";

    int internal_id = sqlite3_column_int(stmt, 0),
      id = sqlite3_column_int(stmt, 1);
    auto _name = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));
    std::string name = _name;
    int x = sqlite3_column_int(stmt, 3),
      y = sqlite3_column_int(stmt, 4),
      width = sqlite3_column_int(stmt, 5),
      height = sqlite3_column_int(stmt, 6),
      group_id = sqlite3_column_int(stmt, 7);
    bool warp_zone = sqlite3_column_int (stmt, 8) == 1;

    pqxx::params p;
    p.append(internal_id);
    p.append(id);
    p.append(name);
    p.append(x);
    p.append(y);
    p.append(width);
    p.append(height);
    p.append(group_id);
    p.append(warp_zone);

    tx.exec(psql_insert, p);

    step_res = sqlite3_step(stmt);
  } while (step_res == SQLITE_ROW);

  sqlite3_finalize(stmt);
  puts("import_objects done\n");
  return true;  
}
  
bool import_objectgroups(pqxx::work &tx, sqlite3 *db) {  
  sqlite3_stmt *stmt;
  std::string query_all = "SELECT * FROM objectgroup";
  int rc = sqlite3_prepare_v2(db, query_all.c_str(), query_all.size(), &stmt, nullptr);

  if(rc != SQLITE_OK) {
    printf("import_objectgroups failed %s\n", sqlite3_errmsg(db));
    return false;
  }

  auto step_res = sqlite3_step(stmt);
  assert(step_res != SQLITE_ERROR);

  do {
    std::string psql_insert = "INSERT INTO objectgroup (internal_id, id, name, map_id) VALUES ($1, $2, $3, $4)";

    int internal_id = sqlite3_column_int(stmt, 0),
      id = sqlite3_column_int(stmt, 1),
      map_id = sqlite3_column_int(stmt, 3);

    auto name_ = sqlite3_column_text(stmt, 2);
    std::string name (reinterpret_cast<const char*>(name_));

    pqxx::params p;
    p.append(internal_id);;
    p.append(id);
    p.append(name);
    p.append(map_id);	

    tx.exec(psql_insert, p);

    step_res = sqlite3_step(stmt);
  } while (step_res == SQLITE_ROW);

  sqlite3_finalize(stmt);
  puts("import_objectgroups done\n");
  return true;  
}

bool import_layers(pqxx::work &tx, sqlite3 *db) {  
  sqlite3_stmt *stmt;
  std::string query_all = "SELECT * FROM layer";
  int rc = sqlite3_prepare_v2(db, query_all.c_str(), query_all.size(), &stmt, nullptr);

  if(rc != SQLITE_OK) {
    printf("import_objectgroups failed %s\n", sqlite3_errmsg(db));
    return false;
  }

  auto step_res = sqlite3_step(stmt);
  assert(step_res != SQLITE_ERROR);

  do {
    std::string psql_insert = "INSERT INTO layer (internal_id, id, name, width, height, map_id) VALUES ($1, $2, $3, $4, $5, $6)";

    int internal_id = sqlite3_column_int(stmt, 0),
      id = sqlite3_column_int(stmt, 1),
      width = sqlite3_column_int(stmt, 3),
      height = sqlite3_column_int(stmt, 4),
      map_id = sqlite3_column_int(stmt, 5);

    auto name_ = sqlite3_column_text(stmt, 2);
    std::string name (reinterpret_cast<const char*>(name_));

    pqxx::params p;
    p.append(internal_id);;
    p.append(id);
    p.append(name);
    p.append(width);
    p.append(height);    
    p.append(map_id);	

    tx.exec(psql_insert, p);

    step_res = sqlite3_step(stmt);
  } while (step_res == SQLITE_ROW);

  sqlite3_finalize(stmt);
  puts("import_layers done\n");
  return true;  
}

bool import_maps(pqxx::work &tx, sqlite3 *db) {  
  sqlite3_stmt *stmt;
  std::string query_all = "SELECT id, tmx_path, png_path, orientation, renderorder, width, height, tilewidth, tileheight, infinite, nextlayerid, nextobjectid, tmx_file FROM map";
  int rc = sqlite3_prepare_v2(db, query_all.c_str(), query_all.size(), &stmt, nullptr);

  if(rc != SQLITE_OK) {
    printf("import_objectgroups failed %s\n", sqlite3_errmsg(db));
    return false;
  }

  auto step_res = sqlite3_step(stmt);
  assert(step_res != SQLITE_ERROR);

  do {
    
    std::string psql_insert = R"(INSERT INTO map (id, tmx_path, png_path, orientation, renderorder, width, height, tilewidth, tileheight, infinite, nextlayerid, nextobjectid, tmx_file )
 VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13 ))";

    int id = sqlite3_column_int(stmt, 0); 
    auto tmx_path_ = sqlite3_column_text(stmt, 1),
      png_path_ = sqlite3_column_text(stmt, 2),
      orientation_ = sqlite3_column_text(stmt, 3),
      renderorder_ = sqlite3_column_text(stmt, 4);

    std::string tmx_path (reinterpret_cast<const char*>(tmx_path_)),
      png_path(reinterpret_cast<const char*>(png_path_)),
      orientation(reinterpret_cast<const char*>(orientation_)),
      renderorder(reinterpret_cast<const char*>(renderorder_));

    int width = sqlite3_column_int(stmt, 5),
      height = sqlite3_column_int(stmt, 6),
      tilewidth =  sqlite3_column_int(stmt, 7),
      tileheight = sqlite3_column_int(stmt, 8);

    bool infinite = sqlite3_column_int(stmt, 9) == 1;
    int nextlayerid= sqlite3_column_int(stmt, 10),
      nextobjectid = sqlite3_column_int(stmt, 11);

    printf("%d, %d,%d,%d,%d, %s, %s, %s, %s \n", id, width, height, tilewidth, tileheight, tmx_path.c_str(), png_path.c_str(), orientation.c_str(), renderorder.c_str());

    const char *tmx_file = reinterpret_cast<const char*>(sqlite3_column_blob(stmt, 12));
    int size = sqlite3_column_bytes(stmt, 12);
    std::string f;

    f.assign(tmx_file, size);

    pqxx::params p;
    p.append(id);
    p.append(tmx_path);
    p.append(png_path);
    p.append(orientation);
    p.append(renderorder);
    p.append(width);
    p.append(height);
    p.append(tilewidth);
    p.append(tileheight);
    p.append(infinite);
    p.append(nextlayerid);
    p.append(nextobjectid);
    p.append(f);
      
    tx.exec(psql_insert, p);

    step_res = sqlite3_step(stmt);
  } while (step_res == SQLITE_ROW);

  puts("Maps done\n");

  sqlite3_finalize(stmt);
  return import_layers(tx, db) && import_objectgroups(tx, db) && import_objects(tx, db) && import_warp_connections(tx, db);
}
