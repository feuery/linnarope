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
( internal_id INTEGER PRIMARY KEY AUTOINCREMENT,
  ID INTEGER NOT NULL,
  name TEXT NOT NULL,
  width INTEGER NOT NULL,
  height INTEGER NOT NULL,
  map_id INTEGER NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE
)")
	 (exec "
CREATE TABLE IF NOT EXISTS objectgroup
( internal_id INTEGER PRIMARY KEY AUTOINCREMENT,
  ID INTEGER,
  name TEXT NOT NULL,
  map_id INTEGER NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE)")

	 (exec "
CREATE TABLE IF NOT EXISTS object
( internal_id INTEGER PRIMARY KEY AUTOINCREMENT,
  id TEXT NOT NULL,
  name TEXT NOT NULL,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  width INTEGER NOT NULL,
  height INTEGER NOT NULL,
  group_id INTEGER NOT NULL REFERENCES objectgroup(ID) ON UPDATE CASCADE ON DELETE CASCADE,
  warp_zone BOOLEAN NOT NULL)
")
	 (exec "
CREATE TABLE IF NOT EXISTS warp_connection
( internal_id INTEGER PRIMARY KEY AUTOINCREMENT,
  src_map INT NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE,
  src_warpzone INT NOT NULL UNIQUE REFERENCES object(internal_id) ON UPDATE CASCADE ON DELETE CASCADE,
  dst_map INT NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE,
  dst_warpzone INT NOT NULL REFERENCES object(internal_id) ON UPDATE CASCADE ON DELETE CASCADE)")

	 (exec "
CREATE TABLE IF NOT EXISTS sprite
(  internal_id INTEGER PRIMARY KEY AUTOINCREMENT,
   png_path TEXT NOT NULL
   -- no references anywhere as these are expected to just be magic handles one can reference in the c++ code, and are thus not bound to a single map
);")
	 (format t "Migrated!~%"))))
