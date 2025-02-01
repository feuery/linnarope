(defpackage linnarope.db.palettes
  (:use :cl)
  (:import-from :cl-hash-util :hash)
  (:export :get-palette :update-rgb :insert-rgb :insert-palette))

(in-package :linnarope.db.palettes)

(defun insert-palette (connection name)
  "Returns the new palette's id"
  (getf
   (cl-dbi:fetch
    (cl-dbi:execute
     (cl-dbi:prepare connection "INSERT INTO palette(name) VALUES(?) RETURNING ID")
     (list name)))
   :ID))

(defun hex-color->rgb (hex-color)
  (let* ((read-hex
	   (parse-integer (str:replace-all "#" "" hex-color) :radix 16))
	 (r (ash read-hex -16))
	 (g (logand (ash read-hex -8) #xff))
	 (b (logand read-hex #xff)))
    (values r g b)))

(defun rgb->hex-color (r g b)
  (format nil "#~6,'0x"
	  (logior 
	   (ash r 16)
	   (ash g 8)
	   b)))

(defun insert-rgb (connection palette-id hex-color)
  (multiple-value-bind (r g b) (hex-color->rgb hex-color)
    (cl-dbi:execute
     (cl-dbi:prepare connection "INSERT INTO palette_color (palette_id, r, g, b) VALUES (?, ?, ?, ?)")
     (list palette-id r g b))))

(defun update-rgb (connection color-id hex-color)
  (multiple-value-bind (r g b) (hex-color->rgb hex-color)
    (cl-dbi:execute
     (cl-dbi:prepare connection "UPDATE palette_color SET r = ?, g = ?, b = ? WHERE ID = ?")
     (list r g b color-id))))

(defun get-palette (connection id)
  (let ((palette-name (getf
		       (cl-dbi:fetch
			(cl-dbi:execute
			 (cl-dbi:prepare connection "SELECT name FROM palette WHERE ID = ?" )
			 (list id)))
		       :|name|))
	(colors (cl-dbi:fetch-all
		 (cl-dbi:execute
		  (cl-dbi:prepare connection "SELECT ID, r, g, b FROM palette_color WHERE palette_id = ?" )
		  (list id)))))
    (assert palette-name)
    (hash ("name" palette-name)
	  ("colors" (mapcar (lambda (c)
			      (format t "Color: ~a~%" c)
			      (let* ((id (getf c :|ID|))
				     (r (getf c :|r|))
				     (g (getf c :|g|))
				     (b (getf c :|b|))
				     (hex (rgb->hex-color r g b)))
				(assert r)
				(assert g)
				(format t "Loading ~a~%" hex)
				(hash ("id" id)
				      ("color" hex)
				      ("name" (format nil "color~d" id)))))
			    colors)))))
