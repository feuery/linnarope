(defpackage linnarope.migrations
  (:use :cl)
  (:import-from :linnarope.middleware :@html :@db)
  (:export :migrate :*tables*))

(in-package :linnarope.migrations)

(defvar *tables* nil)
(defvar *sql-path* (pathname (format nil "~aresources/sql/" (asdf:system-source-directory "linnarope-resource-handler"))))

(defun migrate ()
  (@db (lambda ()
	 (let* ((filename "postgres-migrations.sql")
		(expanded-filename (pathname (format nil "~a/~a" *sql-path* filename)))
		(migration-source
		  (lisp-fixup:slurp-utf-8 expanded-filename)))
	   (postmodern:execute-file expanded-filename)

	   (setf *tables*
		 (map 'list
		      (lisp-fixup:partial #'str:replace-all "CREATE TABLE IF NOT EXISTS " "")
		      (cl-ppcre:all-matches-as-strings "CREATE TABLE (IF NOT EXISTS )?\\w+" migration-source)))))))
