(defpackage linnarope.db.sprites
  (:use :cl)
  (:import-from :lisp-fixup :slurp-bytes :filename )
  (:export :import-sprite!))

(in-package :linnarope.db.sprites)

(defun import-sprite! (png-path)
  (postmodern:execute "INSERT INTO sprite(name, data) VALUES ($1, $2)"
		      (filename png-path) (slurp-bytes png-path)))
