(defpackage linnarope.db.sprites
  (:use :cl)
  (:import-from :lisp-fixup :hashtable-merge)
  (:export :import-sprite!))

(in-package :linnarope.db.sprites)

(defun import-sprite! (connection png-path)
  (cl-dbi:execute
   (cl-dbi:prepare connection "INSERT INTO sprite(png_path) VALUES (?)")
   (list png-path)))
