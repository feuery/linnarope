#include "map_import.h"
#include <map.h>
#include <string>

#define text std::string

bool export_warp_connections(pqxx::work &tx, sqlite3 *db) {
  int counter = 0;
  for(auto [internal_id, src_map, src_warpzone, dst_map, dst_warpzone]:
	tx.query<int, int, int, int, int>("SELECT * FROM warp_connection")) {
    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO warp_connection (internal_id, src_map, src_warpzone, dst_map, dst_warpzone) VALUES (?, ?, ?, ?, ?)";


    int rc = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.length() , &stmt, nullptr);
    if(rc != SQLITE_OK) {
      printf("fail 5: %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_int(stmt, 1, internal_id);
    sqlite3_bind_int(stmt, 2, src_map);
    sqlite3_bind_int(stmt, 3, src_warpzone);
    sqlite3_bind_int(stmt, 4, dst_map);
    sqlite3_bind_int(stmt, 5, dst_warpzone);

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("fail 6: %s\n", sqlite3_errmsg(db));
      return false;
    }
    else counter++;
    
    sqlite3_finalize(stmt);
  }

  printf("Exported %d warp connections\n", counter);

  return true;
}

bool export_objects(pqxx::work &tx, sqlite3 *db) {
  int counter = 0;
  for(auto [internal_id, id, name, x, y, width, height, group_id, warp_zone]:
	tx.query<int, text, text, int, int, int, int, int, bool>("SELECT * FROM object")) {
    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO object (internal_id, id, name, x, y, width, height, group_id, warp_zone) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";


    int rc = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.length() , &stmt, nullptr);
    if(rc != SQLITE_OK) {
      printf("fail 5: %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_int(stmt, 1, internal_id);
    sqlite3_bind_text(stmt, 2, id.c_str(), id.size(), SQLITE_STATIC);
    sqlite3_bind_text(stmt, 3, name.c_str(), name.size(), SQLITE_STATIC);
    sqlite3_bind_int(stmt, 4, x);
    sqlite3_bind_int(stmt, 5, y);
    sqlite3_bind_int(stmt, 6, width);
    sqlite3_bind_int(stmt, 7, height);
    sqlite3_bind_int(stmt, 8, group_id);
    sqlite3_bind_int(stmt, 9, warp_zone);    

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("fail 6: %s\n", sqlite3_errmsg(db));
      return false;
    }
    else counter++;
    
    sqlite3_finalize(stmt);
  }

  printf("Exported %d objects\n", counter);

  return true;
}

bool export_objectgroups(pqxx::work &tx, sqlite3 *db) {
  for(auto [internal_id, id, name, map_id]:
	tx.query<int, int, std::string, int>("SELECT * FROM objectgroup")) {
    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO objectgroup (internal_id, ID, name, map_id) VALUES (?, ?, ?, ?)";


    int rc = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.length() , &stmt, nullptr);
    if(rc != SQLITE_OK) {
      printf("fail 5: %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_int(stmt, 1, internal_id);
    sqlite3_bind_int(stmt, 2, id);
    sqlite3_bind_text(stmt, 3, name.c_str(), name.size(), SQLITE_STATIC);
    sqlite3_bind_int(stmt, 4, map_id);

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("fail 6: %s\n", sqlite3_errmsg(db));
      return false;
    }
    else printf("Exported obgroup %s\n", name.c_str());
    
    sqlite3_finalize(stmt);
  }

  return true;
}

bool export_layers(pqxx::work &tx, sqlite3 *db) {
  for(auto [internal_id, id, name, width, height, map_id]:
	tx.query<int, int, std::string, int, int, int>("SELECT * FROM layer")) {
    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO layer (internal_id, ID, name, width, height, map_id) VALUES (?, ?, ?, ?, ?, ?)";


    int rc = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.length() , &stmt, nullptr);
    if(rc != SQLITE_OK) {
      printf("fail 5: %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_int(stmt, 1, internal_id);
    sqlite3_bind_int(stmt, 2, id);
    sqlite3_bind_text(stmt, 3, name.c_str(), name.size(), SQLITE_STATIC);
    sqlite3_bind_int(stmt, 4, width);
    sqlite3_bind_int(stmt, 5, height);
    sqlite3_bind_int(stmt, 6, map_id);

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("fail 6: %s\n", sqlite3_errmsg(db));
      return false;
    }
    else printf("Exported %s\n", name.c_str());
    
    sqlite3_finalize(stmt);
  }

  return true;
}

bool export_images(pqxx::work &tx, sqlite3 *db) {
  for(auto [filename, img]: tx.query<std::string, pqxx::bytes>("SELECT filename, img FROM image_file")) {
    sqlite3_stmt *stmt;
    std::string insert = "INSERT INTO image_file (filename, img) VALUES(?, ?)";
    int prepd = sqlite3_prepare_v2(db, insert.c_str(), insert.size(), &stmt, nullptr);

    if(prepd != SQLITE_OK) {
      printf("Export images failed (1) %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_text(stmt, 1, filename.c_str(), filename.size(), SQLITE_STATIC);
    sqlite3_bind_blob(stmt, 2, img.data(), img.size(), SQLITE_STATIC);

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("Export images failed %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_finalize(stmt);
    
  }
  return true;
}

bool export_tilesets(pqxx::work &tx, sqlite3 *db)
{
  for (auto [filename, data]: tx.query<std::string, pqxx::bytes>("SELECT filename, tsx_contents FROM tileset")) {
    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO tileset(filename, tsx_contents) VALUES (?, ?)";

    int prepared = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.length(), &stmt, nullptr);

    if (prepared != SQLITE_OK) {
      printf("Export tilesets failed at the tileset stage %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_text(stmt, 1, filename.c_str(), filename.size(), SQLITE_STATIC);
    sqlite3_bind_blob(stmt, 2, data.data(), data.size(), SQLITE_STATIC);

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("failed the actual insertion of tileset: %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_finalize(stmt);
  }


  // the many-2-many table 
  for (auto [id, map_id, tileset_filename]:
	 tx.query<int, int, std::string>("SELECT id, map_id, tileset_filename FROM map_to_tileset")) {
    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO map_to_tileset(id, map_id, tileset_filename) VALUES (?, ?, ?)";

    int prepared = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.length(), &stmt, nullptr);

    if (prepared != SQLITE_OK) {
      printf("Export tilesets failed at the map_to_tileset stage %s\n", sqlite3_errmsg(db));
      return false;
    }

    sqlite3_bind_int(stmt, 1, id);
    sqlite3_bind_int(stmt, 2, map_id);
    sqlite3_bind_text(stmt, 3, tileset_filename.c_str(), tileset_filename.size(), SQLITE_STATIC);

    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("failed the actual insertion of map_to_tileset: %s\n", sqlite3_errmsg(db));
      return false;
    }
    sqlite3_finalize(stmt);
  }

  return export_images(tx, db);
}

bool export_maps(pqxx::work &tx, sqlite3 *db) {
  for(auto [id, tmx_path, png_path, orientation, renderorder, width, height, tilewidth, tileheight, infinite, nextlayerid, nextobjectid, tmx_file]:
	tx.query<int, std::string,  std::string, std::string, std::string,
	int, int, int, int, bool, int, int, pqxx::bytes>("SELECT * FROM map")) {
    printf("exporting: %s\n", tmx_path.c_str());

    sqlite3_stmt *stmt;
    std::string insert_q = "INSERT INTO map (id, tmx_path, png_path, orientation, renderorder, width, height, tilewidth, tileheight, infinite, nextlayerid, nextobjectid, tmx_file) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
    int rc = sqlite3_prepare_v2(db, insert_q.c_str(), insert_q.length() , &stmt, nullptr);

    if(rc != SQLITE_OK) {
      printf("fail 4: %s\n", sqlite3_errmsg(db));
      return false;
    }
    
    sqlite3_bind_int(stmt, 1, id);
    sqlite3_bind_text(stmt, 2, tmx_path.c_str(), tmx_path.size(), SQLITE_STATIC);
    sqlite3_bind_text(stmt, 3, png_path.c_str(), png_path.size(), SQLITE_STATIC);
    sqlite3_bind_text(stmt, 4, orientation.c_str(), orientation.size(),
                      SQLITE_STATIC);
    sqlite3_bind_text(stmt, 5, renderorder.c_str(), renderorder.size(), SQLITE_STATIC);
    sqlite3_bind_int(stmt, 6, width);
    sqlite3_bind_int(stmt, 7, height);
    sqlite3_bind_int(stmt, 8, tilewidth);
    sqlite3_bind_int(stmt, 9, tileheight);
    sqlite3_bind_int(stmt, 10, infinite);
    sqlite3_bind_int(stmt, 11, nextlayerid);
    sqlite3_bind_int(stmt, 12, nextobjectid);
    sqlite3_bind_blob(stmt, 13, tmx_file.data(), tmx_file.size(), SQLITE_STATIC);


    if(sqlite3_step(stmt) == SQLITE_ERROR) {
      printf("fail 3: %s\n", sqlite3_errmsg(db));
      return false;
    }
    else printf("Exported %s\n", tmx_path.c_str());
    
    sqlite3_finalize(stmt);
  }
  return export_layers(tx, db) && export_objectgroups(tx, db) && export_objects(tx, db) && export_warp_connections(tx, db) && export_tilesets(tx, db);
}
