#include <stdio.h>
#include <pqxx/pqxx>
#include <string>
#include <sqlite3.h>

// postgresql://[userspec@][hostspec][/dbname][?paramspec]

// where userspec is:

// user[:password]

// and hostspec is:

// [host][:port][,...]

// and paramspec is:

// name=value[&...]


const char *user = "linnarope", *password = "linnarope", *db = "linnarope";
std::vector<std::string> ddls = {"CREATE TABLE IF NOT EXISTS map \
( ID INTEGER PRIMARY KEY AUTOINCREMENT, \
  tmx_path TEXT UNIQUE, \
  png_path TEXT UNIQUE, \
  orientation TEXT NOT NULL, \
  renderorder TEXT NOT NULL, \
  width INTEGER NOT NULL, \
  height INTEGER NOT NULL, \
  tilewidth INTEGER NOT NULL, \
  tileheight INTEGER NOT NULL, \
  infinite BOOL NOT NULL, \
  nextlayerid INTEGER NOT NULL, \
  nextobjectid INTEGER NOT NULL   \
  )", 
				 "CREATE TABLE IF NOT EXISTS layer \
( internal_id INTEGER PRIMARY KEY AUTOINCREMENT, \
  ID INTEGER NOT NULL, \
  name TEXT NOT NULL, \
  width INTEGER NOT NULL, \
  height INTEGER NOT NULL, \
  map_id INTEGER NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE \
)", 
				 "CREATE TABLE IF NOT EXISTS objectgroup \
( internal_id INTEGER PRIMARY KEY AUTOINCREMENT, \
  ID INTEGER, \
  name TEXT NOT NULL, \
  map_id INTEGER NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE)", 
				 "\
	 CREATE TABLE IF NOT EXISTS object \
( internal_id INTEGER PRIMARY KEY AUTOINCREMENT, \
  id TEXT NOT NULL, \
  name TEXT NOT NULL, \
  x INTEGER NOT NULL, \
  y INTEGER NOT NULL, \
  width INTEGER NOT NULL, \
  height INTEGER NOT NULL, \
  group_id INTEGER NOT NULL REFERENCES objectgroup(ID) ON UPDATE CASCADE ON DELETE CASCADE, \
  warp_zone BOOLEAN NOT NULL)", 
				 "CREATE TABLE IF NOT EXISTS warp_connection \
( internal_id INTEGER PRIMARY KEY AUTOINCREMENT, \
  src_map INT NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE, \
  src_warpzone INT NOT NULL UNIQUE REFERENCES object(internal_id) ON UPDATE CASCADE ON DELETE CASCADE, \
  dst_map INT NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE, \
  dst_warpzone INT NOT NULL REFERENCES object(internal_id) ON UPDATE CASCADE ON DELETE CASCADE)", 
				 "\
	 CREATE TABLE IF NOT EXISTS sprite \
(  internal_id INTEGER PRIMARY KEY AUTOINCREMENT, \
   png_path TEXT NOT NULL \
)", 
				 "CREATE TABLE IF NOT EXISTS palette \
(  ID INTEGER PRIMARY KEY AUTOINCREMENT, \
   name TEXT UNIQUE NOT NULL, \
   color_array JSONB NOT NULL)", 
				 \
				 "CREATE TABLE IF NOT EXISTS lisp_sprite \
( ID INTEGER PRIMARY KEY AUTOINCREMENT, \
  name TEXT NOT NULL, \
  w INT NOT NULL CHECK (w > 0), \
  h INT NOT NULL CHECK (h > 0), \
 \
  palette_id INT NOT NULL REFERENCES palette(ID) \
  ON UPDATE CASCADE \
  ON DELETE RESTRICT \
  )", 
				 "CREATE TABLE IF NOT EXISTS lisp_sprite_pixel \
( ID INTEGER PRIMARY KEY AUTOINCREMENT, \
  sprite_id ID NOT NULL REFERENCES lisp_sprite(ID) ON DELETE CASCADE ON UPDATE CASCADE, \
  x INTEGER NOT NULL, \
  y INTEGER NOT NULL, \
  color_index INT NOT NULL DEFAULT 0)"};

#define connection_string()                                                    \
  std::string("postgresql://") + user + ":" + password + "@localhost:5432/" + db

bool exec_ddl(sqlite3 *db, const char *sql) {
  sqlite3_stmt *stmt;

  int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr);
  if(rc != SQLITE_OK) {
    printf("fail 1: %s\n", sqlite3_errmsg(db));
    return false;
  }

  if(sqlite3_step(stmt) == SQLITE_ERROR) {
    printf("fail 2: %s\n", sqlite3_errmsg(db));
    return false;
  }

  sqlite3_finalize(stmt);
  return true;
}


int main(int argc, char **argv) {

  if(argc != 2) {
    printf("Usage: exporter projectfilename\n");
    return -1;
  }

  std::string connString = connection_string(),
    dst_sqlite_path = argv[1];

  sqlite3 *db;
  sqlite3_open(dst_sqlite_path.c_str(), &db);
  
  
  pqxx::connection c(connString);
  pqxx::work w{c};
  for(auto [id, tmx_path, png_path, orientation, renderorder]: w.query<int, std::string,std::string,std::string,std::string>("SELECT id, tmx_path, png_path, orientation, renderorder FROM map")) {
    printf("Map data: %d, %s, %s, %s, %s\n ", id, tmx_path.c_str(), png_path.c_str(), orientation.c_str(), renderorder.c_str());
  }

  for(auto ddl: ddls) {
    exec_ddl(db, ddl.c_str());
  }

  puts("Done all the tables\n");
  puts("sqlite-kannan pitäisi olla nyt olemassa?");

  sqlite3_close(db);
  
  return 0;
}
