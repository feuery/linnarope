#include <cstddef>
#include <teardowner.h>
#include <pqxx/pqxx>
#include <string>
#include <sqlite3.h>
#include <app.h>
#include <unistd.h>

#include <gtest/gtest.h>

std::string get_file_contents(const char *filename) {

  FILE *fp = fopen(filename, "rb");
  if (fp)
    {
      std::string contents;
      fseek(fp, 0, SEEK_END);
      contents.resize(ftell(fp));
      rewind(fp);
      fread(&contents[0], 1, contents.size(), fp);
      fclose(fp);
      return(contents);
    }

  printf("get_file_contents('%s'); failed\n", filename);
  throw(errno);
}


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

TEST(exporting_and_importing_sprites, all) {
  std::string test_sqlite_url = "./test.export",
    psql_connstring = test_connection_string();

  Teardowner t([&]() {
    puts("Trying to remove sqlite");
    if (access(test_sqlite_url.c_str(), F_OK) == 0) {
      remove(test_sqlite_url.c_str());
      puts("Removed sqlite");
    }	
  });

  ASSERT_EQ(1, 0);
  
  
  printf("Using test postgres in %s\n", psql_connstring.c_str());
  pqxx::connection c(psql_connstring);
  pqxx::work tx {c};
  
  setup_test_db(tx);
  puts("setup_test_db succeeded");

  setup_sprites(tx);
  puts("setup_sprites() succeeded");

  auto [sprite_count] = *tx.query<int>("SELECT COUNT(*) FROM lisp_sprite").begin();
  auto [palette_count] = *tx.query<int>("SELECT COUNT(*) FROM palette").begin();

  ASSERT_EQ(sprite_count, 1);
  ASSERT_EQ(palette_count, 1);

  tx.commit();

  // SECTION("test export") {

  // sqlite export doesn't exist
  ASSERT_NE(access(test_sqlite_url.c_str(), F_OK), 0);
  Exporter ex;

  ex.do_it(psql_connstring, test_sqlite_url);

  // sqlite export exists
  ASSERT_EQ(access(test_sqlite_url.c_str(), F_OK), 0);

  {
    sqlite3 *db;
    sqlite3_stmt *stmt;

    Teardowner t ([&]() {
      sqlite3_finalize(stmt);
      sqlite3_close_v2(db);
    });
    
    
    sqlite3_open(test_sqlite_url.c_str(), &db);
    std::string sprite_pixels_q = "SELECT pixels FROM lisp_sprite WHERE id = 0";

    sqlite3_prepare_v2(db, sprite_pixels_q.c_str(), sprite_pixels_q.size(), &stmt, nullptr);

    int step_res;
    int c = 0;
    while((step_res = sqlite3_step(stmt)) == SQLITE_ROW) {
      auto pixels_ = sqlite3_column_text(stmt, 0);
      std::string pixels(reinterpret_cast<const char*>(pixels_));

      ASSERT_EQ(pixels, "[[0, 1, 2, 3], [0, 4, 4, 4], [4, 4, 2, 3], [0, 3, 4, 3], [0, 3, 4, 3], [0, 3, 4, 3], [0, 3, 4, 3], [3, 3, 4, 3], [3, 3, 2, 3], [0, 1, 2, 3], [0, 1, 2, 3], [0, 1, 2, 3]]");
      c++;
    }

    if (step_res == SQLITE_ERROR) {
      printf("sqlite error: %s\n", sqlite3_errmsg(db));
    }
    
    ASSERT_EQ(c, 1); 		// should've found a single row


    // TODO: what if we pull the export back without emptying psql?
    // TODO: what if wwe re-export it with the same name? Will the data stay intact?
  }
  
}
