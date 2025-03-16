(defpackage linnarope.db.palettes
  (:use :cl)
  (:import-from :cl-hash-util :hash)
  (:local-nicknames (:json :com.inuoe.jzon))
  (:export :update-palette-colors :get-palette :insert-palette))

(in-package :linnarope.db.palettes)

(defun insert-palette (name color-array)
  "Returns the new palette's id.

Asserts that color-array is really #(an array), which is necessary due to json:stringify"  
  (assert (arrayp color-array))
  (caar (postmodern:query "INSERT INTO palette(name, color_array) VALUES($1, $2) RETURNING ID" name (json:stringify color-array))))

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

(defun get-palette (id)
  (let* ((palette (aref (postmodern:query "SELECT name, color_array FROM palette WHERE ID = $1" id :array-hash) 0))
	 (palette-name (gethash "name" palette))
	 ;; returns #("FF00AA" "C0FFEE" "123456" ....)
	 (colors (json:parse (gethash "color_array" palette))))
    (hash ("name" palette-name)
	  ("colors" colors))))

(defun update-palette-colors (id colors)
  (let ((colors (json:stringify (if (vectorp colors)
				    colors
				    (coerce colors 'vector)))))
    (postmodern:execute "UPDATE palette SET color_array = $1 WHERE ID = $2" colors id)))
