#include <cstddef>
#include <teardowner.h>
#include <pqxx/pqxx>
#include <string>
#include <sqlite3.h>
#include <app.h>
#include <lib_fixup.h>

#include <sprite_test.h>

void setup_test_db(pqxx::work &tx) {
  tx.exec(get_file_contents("./resource_handler/resources/sql/postgres-migrations.sql")).no_rows();
}

void setup_sprites (pqxx::work& tx) {  
  
  std::string insert_sprite_query = R"(INSERT INTO lisp_sprite (id, name, w, h, palette_id, pixels)
 VALUES
($1, $2, $3, $4, $5, $6) ON CONFLICT DO NOTHING)",
    insert_palette_query = R"(INSERT INTO palette
(id, name, color_array)
 VALUES
($1, $2, $3) ON CONFLICT DO NOTHING)";

  pqxx::params sprite_params;
  sprite_params.append(0);
  sprite_params.append("testisprite");
  sprite_params.append(4);
  sprite_params.append(4);
  // palette_id
  sprite_params.append(0);
  sprite_params.append("[[0, 1, 2, 3], [0, 4, 4, 4], [4, 4, 2, 3], [0, 3, 4, 3], [0, 3, 4, 3], [0, 3, 4, 3], [0, 3, 4, 3], [3, 3, 4, 3], [3, 3, 2, 3], [0, 1, 2, 3], [0, 1, 2, 3], [0, 1, 2, 3]]");


  pqxx::params palette_params;
  palette_params.append(0);
  palette_params.append("testipaletti");
  palette_params.append(R"(["#000000", "#ffffff", "#ff2600", "#0433ff", "#00f900"])");

  puts("calling insert_palette");
  tx.exec(insert_palette_query, palette_params);

  puts("calling insert_sprites");
  tx.exec(insert_sprite_query, sprite_params);
 
}
