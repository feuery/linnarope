(defpackage linnarope.middleware
  (:use :cl)
  (:import-from :lisp-fixup :with-output-to-real-string)
  (:export :restored-stdout :@html :@db :*connection*))

(in-package :linnarope.middleware)

(defvar *stdout* nil)

(defun doctype (body)
  (format nil "<!doctype html>~%~a" body))

(defun @html (next)
  (let ((*stdout* *standard-output*))
    (doctype
     (with-output-to-real-string
	 (funcall next)))))

(defmacro restored-stdout (&rest body)
  "Restores (format nil) to output to stdout inside a @html handler"
  `(let ((*standard-output* (or *stdout* *standard-output*)))
     ,@body))

;; database
(defvar *system-source-directory*
  (asdf:system-source-directory "linnarope-resource-handler"))

(defvar *connection*
  nil)

(defun @db (next)
  (dbi:with-connection (conn :sqlite3 :database-name (format nil "~aresources.db" *system-source-directory*))
    (let ((*connection* conn))
      (funcall next))))
