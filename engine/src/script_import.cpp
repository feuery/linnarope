#include "lib_fixup.h"
#include <cassert>
#include "tmx_private.h"
#include <app.h>
#include <filesystem>
#include <string>

ScriptImport::ScriptImport(std::string project, std::string src): src_dir(src), project_path(project) {}

void ScriptImport::do_it() {
  sqlite3 *db;
  auto result = sqlite3_open(project_path.c_str(), &db);

  if (result != SQLITE_OK) {
    printf("Opening %s failed due to %s\n", project_path.c_str(), sqlite3_errmsg(db));
    return;
  }

  sqlite3_stmt *stmt;
  /*std::string del = "DELETE FROM script";
  sqlite3_prepare_v2(db, del.c_str(), del.size(), &stmt, nullptr);

  result = sqlite3_step(stmt);
  if (result == SQLITE_ERROR) {
    printf("Deleting old scripts failed due to %s\n", sqlite3_errmsg(db));
    return;
  }

  sqlite3_finalize(stmt); */

  for(auto& entry: std::filesystem::directory_iterator(src_dir)) {
    auto p = entry.path();
    printf("Importing script %s\n", p.filename().c_str());

    std::string script_fname = src_dir+"/"+p.filename().string(),
      script_contents = get_file_contents(script_fname.c_str());

    if(!script_fname.ends_with(".lisp")) {
      printf("%s doesn't end with .lisp, skipping\n", script_fname.c_str());
      continue;
    }

    std::string upsert = "INSERT INTO script (name, script) VALUES (?, ?) ON CONFLICT DO UPDATE SET script = excluded.script";
    sqlite3_prepare_v2(db, upsert.c_str(), upsert.size(), &stmt, nullptr);

    sqlite3_bind_text(stmt, 1, p.filename().string().c_str(), p.filename().string().size(), SQLITE_STATIC);
    sqlite3_bind_text(stmt, 2, script_contents.c_str(), script_contents.size(), SQLITE_STATIC);

    result = sqlite3_step(stmt);

    if(result != SQLITE_DONE) {
      printf("step failed, db errmsg %s\n", sqlite3_errmsg(db));
      return;
    }
    assert(result == SQLITE_DONE);
    sqlite3_finalize(stmt);
  }

  sqlite3_close(db);

  puts("Import succesful\n");
}
