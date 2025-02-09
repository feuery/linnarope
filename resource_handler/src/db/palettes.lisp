(defpackage linnarope.db.palettes
  (:use :cl)
  (:import-from :cl-hash-util :hash)
  (:local-nicknames (:json :com.inuoe.jzon))
  (:export :update-palette-colors :get-palette :insert-palette))

(in-package :linnarope.db.palettes)

(defun insert-palette (connection name color-array)
  "Returns the new palette's id.

Asserts that color-array is really #(an array), which is necessary due to json:stringify"  
  (assert (arrayp color-array))
  (getf
   (cl-dbi:fetch
    (cl-dbi:execute
     (cl-dbi:prepare connection "INSERT INTO palette(name, color_array) VALUES(?, ?) RETURNING ID")
     (list name (json:stringify color-array))))
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

(defun get-palette (connection id)
  (let* ((palette (cl-dbi:fetch
		   (cl-dbi:execute
		    (cl-dbi:prepare connection "SELECT name, color_array FROM palette WHERE ID = ?" )
		    (list id))))
	 (palette-name (getf palette
			     :|name|))
	 ;; returns #("FF00AA" "C0FFEE" "123456" ....)
	 (colors (json:parse (getf palette
				   :|color_array|))))
    (hash ("name" palette-name)
	  ("colors" colors))))

(defun update-palette-colors (connection id colors)
  (let ((colors (json:stringify (if (vectorp colors)
				    colors
				    (coerce colors 'vector)))))
    (cl-dbi:execute (cl-dbi:prepare connection "UPDATE palette SET color_array = ? WHERE ID = ?")
		    (list colors id))))
