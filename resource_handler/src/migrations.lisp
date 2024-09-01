(defpackage linnarope.migrations
  (:use :cl)
  (:import-from :linnarope.middleware :@html :@db :*connection*)
  (:export :migrate))

(in-package :linnarope.migrations)

(defun exec (str)
  (cl-dbi:execute (cl-dbi:prepare *connection* str)))

(defun migrate ()
  (@db (lambda ()
	 (exec "CREATE TABLE IF NOT EXISTS map ( ID INTEGER PRIMARY KEY AUTOINCREMENT, tmx_path TEXT UNIQUE, png_path TEXT UNIQUE)")
	 
	 (format t "Migrated!~%"))))
