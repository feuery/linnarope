(defpackage linnarope.migrations
  (:use :cl)
  (:import-from :linnarope.middleware :@html :@db)
  (:export :migrate :*tables*))

(in-package :linnarope.migrations)

(defun exec (str)
  (postmodern:execute str))

(defvar *tables* nil)

(defun create-table (name ddl)
  (exec ddl)
  (unless (member name *tables* :test 'equal)
    (push name *tables*)))

(defun migrate ()
  (@db (lambda ()
	 (format t "Running migrations")
	 (create-table "map" "
CREATE TABLE IF NOT EXIsts map
( ID SERIAL PRIMARY KEY,
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
  nextobjectid INTEGER NOT NULL,
  -- the actual tmx file, you can't exactly reconstruct it with the minimal tables we have defined here.
  tmx_file BYTEA NOT NULL
)")
	 (create-table "tileset"
		       "
CREATE TABLE IF NOT EXISTS tileset
(  --maps refer to tilesets by filename.tsx without parent directories
   filename TEXT PRIMARY KEY,
   tsx_contents BYTEA NOT NULL)")
	 (create-table "map_to_tileset"
		 "
CREATE TABLE IF NOT EXISTS map_to_tileset
( ID SERIAL PRIMARY KEY,
  map_id INTEGER NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE,
  tileset_filename TEXT NOT NULL REFERENCES tileset(filename) ON UPDATE CASCADE ON DELETE CASCADE)")
	 (create-table "image_file"
"CREATE TABLE IF NOT EXISTS image_file
( filename TEXT NOT NULL,
  img BYTEA NOT NULL)")	 
	 (create-table "layer" "
CREATE TABLE IF NOT EXISTS layer
( internal_id SERIAL PRIMARY KEY,
  ID INTEGER NOT NULL,
  name TEXT NOT NULL,
  width INTEGER NOT NULL,
  height INTEGER NOT NULL,
  map_id INTEGER NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE
)")
	 (create-table "objectgroup" "
CREATE TABLE IF NOT EXISTS objectgroup
( internal_id SERIAL PRIMARY KEY,
  ID INTEGER,
  name TEXT NOT NULL,
  map_id INTEGER NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE)")

	 (create-table "object" "
CREATE TABLE IF NOT EXISTS object
( internal_id SERIAL PRIMARY KEY,
  id TEXT NOT NULL,
  name TEXT NOT NULL,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  width INTEGER NOT NULL,
  height INTEGER NOT NULL,
  group_id INTEGER NOT NULL REFERENCES objectgroup(internal_id) ON UPDATE CASCADE ON DELETE CASCADE,
  warp_zone BOOLEAN NOT NULL)
")
	 (create-table "warp_connection" "
CREATE TABLE IF NOT EXISTS warp_connection
( internal_id SERIAL PRIMARY KEY,
  src_map INT NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE,
  src_warpzone INT NOT NULL UNIQUE REFERENCES object(internal_id) ON UPDATE CASCADE ON DELETE CASCADE,
  dst_map INT NOT NULL REFERENCES map(ID) ON UPDATE CASCADE ON DELETE CASCADE,
  dst_warpzone INT NOT NULL REFERENCES object(internal_id) ON UPDATE CASCADE ON DELETE CASCADE)")

	 (create-table "sprite" "
CREATE TABLE IF NOT EXISTS sprite
(  internal_id SERIAL PRIMARY KEY,
   name TEXT NOT NULL,
   data BYTEA NOT NULL
   -- no references anywhere as these are expected to just be magic handles one can reference in the c++ code, and are thus not bound to a single map
);")
	 (create-table
	  "palette" "CREATE TABLE IF NOT EXISTS palette
(  ID SERIAL PRIMARY KEY,
   name TEXT UNIQUE NOT NULL,
   color_array JSONB NOT NULL)")
	 
	 (create-table
	  "lisp_sprite" "CREATE TABLE IF NOT EXISTS lisp_sprite
( ID SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  w INT NOT NULL CHECK (w > 0),
  h INT NOT NULL CHECK (h > 0),

  palette_id INT NOT NULL REFERENCES palette(ID)
  ON UPDATE CASCADE
  -- deleting palettes should prompt the user update palettes and not `rm -rf` this table
  ON DELETE RESTRICT
  -- we'll see if thiqs table should be made into view that pulls the hex-color straight from the palette table
)")

	 (create-table "lisp_sprite_pixel" "CREATE TABLE IF NOT EXISTS lisp_sprite_pixel
( ID SERIAL PRIMARY KEY,
  sprite_id INT NOT NULL REFERENCES lisp_sprite(ID) ON DELETE CASCADE ON UPDATE CASCADE,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  color_index INT NOT NULL DEFAULT 0)")
	 (format t "Migrated!~%"))))
