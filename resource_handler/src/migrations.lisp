(defpackage linnarope.migrations
  (:use :cl)
  (:import-from :linnarope.middleware :@html :@db :*connection*)
  (:export :migrate))

(in-package :linnarope.migrations)

(defun exec (str)
  (cl-dbi:execute (cl-dbi:prepare *connection* str)))

(defun migrate ()
  (@db (lambda ()
	 (exec "CREATE TABLE IF NOT EXISTS map ( ID INTEGER PRIMARY KEY AUTOINCREMENT, path TEXT NOT NULL)")

	 ;; write new migrations with that format as idempotent sql

	 
	 (format t "Migrated!~%"))))
