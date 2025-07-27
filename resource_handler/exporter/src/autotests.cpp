#include <result.h>
#include <app.h>
#include <pqxx/pqxx>

#include <test.h>
#include <sprite_test.h>
#include <unistd.h>
#include <vector>
#include <sqlite3.h>
#include <teardowner.h>

std::vector<Test> AutoTests::get_tests() {
  std::vector<Test> acc;
  std::string psql_connstring = postgres_connstring();
  std::string test_sqlite_url = "./test.export";

  acc.push_back(Test([=](std::vector<Result>& results){
    printf("Using test postgres in %s\n", psql_connstring.c_str());
    pqxx::connection c(psql_connstring);
    pqxx::work tx {c};

    setup_test_db(tx);

    puts("setup_test_db succeeded");

    setup_sprites(tx);
    puts("setup_sprites() succeeded");

    // above should probably be moved to the setup lambda...

    auto [sprite_count] = *tx.query<int>("SELECT COUNT(*) FROM lisp_sprite").begin();
    auto [palette_count] = *tx.query<int>("SELECT COUNT(*) FROM palette").begin();

    ENSURE(sprite_count == 1);
    ENSURE(palette_count == 1);

    tx.commit();


    // sqlite export doesn't exist
    ENSURE(access(test_sqlite_url.c_str(), F_OK) != 0);
    Exporter ex;
    ex.isTest = true;
    ex.do_it(test_sqlite_url);

    // sqlite export exists
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
    int count = 0;
    while((step_res = sqlite3_step(stmt)) == SQLITE_ROW) {
      auto pixels_ = sqlite3_column_text(stmt, 0);
      std::string pixels(reinterpret_cast<const char*>(pixels_));

      ENSURE(pixels == "[[0, 1, 2, 3], [0, 4, 4, 4], [4, 4, 2, 3], [0, 3, 4, 3], [0, 3, 4, 3], [0, 3, 4, 3], [0, 3, 4, 3], [3, 3, 4, 3], [3, 3, 2, 3], [0, 1, 2, 3], [0, 1, 2, 3], [0, 1, 2, 3]]");
      count++;
    }

    if (step_res == SQLITE_ERROR) {
      printf("sqlite error: %s\n", sqlite3_errmsg(db));
    }
    
    ENSURE(count == 1); 		 //should've found a single row

    /* TODO: what if we pull the export back without emptying psql?
       TODO: what if wwe re-export it with the same name? Will the data stay intact?*/

    return true;
    
  },
      [=](){
	if (access(test_sqlite_url.c_str(), F_OK) == 0) {
	  remove(test_sqlite_url.c_str());
	}
	return true;
      },
      [=](){
	puts("Trying to remove sqlite");
	if (access(test_sqlite_url.c_str(), F_OK) == 0) {
	  remove(test_sqlite_url.c_str());
	  puts("Removed sqlite");
	}

	return true;
      },
      "sprite-tests or smthing"
      ));
  
  return acc;
}

AutoTests::AutoTests(Reporter rep): reporter(rep) {
  isTest = true;
}

void AutoTests::do_it(std::string sqlite_path) { RunAndReportTests(); }

void AutoTests::RunAndReportTests() {
  for(auto &test: get_tests()) {
    test.setup();

    test.run_test();
    test.report(reporter);
    // auto result = test.run_test();
    // printf("%s => %s\n", test.name().c_str(), result.result? "SUCCESS":"FAILURE");

    test.teardown();
  }
}
