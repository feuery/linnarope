(defpackage linnarope.migrations
  (:use :cl)
  (:import-from :linnarope.middleware :@html :@db :*connection*)
  (:export :migrate))

(in-package :linnarope.migrations)

(defun exec (str)
  (cl-dbi:execute (cl-dbi:prepare *connection* str)))

(defun migrate ()
  (@db (lambda ()
	 (exec "
CREATE TABLE IF NOT EXISTS map
( ID INTEGER PRIMARY KEY AUTOINCREMENT,
  tmx_path TEXT UNIQUE,
  png_path TEXT UNIQUE,
  orientation TEXT NOT NULL,
  renderorder TEXT NOT NULL,
  width INTEGER NOT NULL,
  height INTEGER NOT NULL,
  tilewidth INTEGER NOT NULL,
  tileheight INTEGER NOT NULL,
  infinite BOOL NOT NULL,
  nextlayerid INTEGER NOT NULL,
  nextobjectid INTEGER NOT NULL  
)")
	 (exec "
CREATE TABLE IF NOT EXISTS layer
( ID INTEGER PRIMARY KEY NOT NULL,
  name TEXT NOT NULL,
  width INTEGER NOT NULL,
  height INTEGER NOT NULL,
  map_id INTEGER NOT NULL,
  FOREIGN KEY (map_id) REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE
)")
	 (exec "
CREATE TABLE IF NOT EXISTS objectgroup
( ID INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  map_id INTEGER NOT NULL,
  FOREIGN KEY (map_id) REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE)")

	 (exec "
CREATE TABLE IF NOT EXISTS object
( id TEXT PRIMARY KEY NOT NULL,
  name TEXT NOT NULL,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  width INTEGER NOT NULL,
  height INTEGER NOT NULL,
  group_id INTEGER NOT NULL,
  warp_zone BOOLEAN NOT NULL,
  FOREIGN KEY(group_id) REFERENCES objectgroup(ID) ON UPDATE CASCADE ON DELETE CASCADE)
")
	 (format t "Migrated!~%"))))
