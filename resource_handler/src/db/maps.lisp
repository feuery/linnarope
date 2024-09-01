(defpackage linnarope.db.maps
  (:use :cl)
  (:export :save-map-to-db!))

(in-package :linnarope.db.maps)

(defvar *engine-binary-location* (pathname
				  (format nil "~afinropedemo"
				  (uiop:pathname-parent-directory-pathname
				   (asdf:system-source-directory "linnarope-resource-handler")))))

(assert 
 (cl-fad:file-exists-p *engine-binary-location*))

(defun generate-png-filename (path)
  (format nil "~a.resource-handler.png" path))

;; (uiop:pathname-directory-pathname tmx-path)
(defun generate-png (tmx-path)
  (sb-ext:run-program *engine-binary-location*
		      (list "--map-file" tmx-path "--png-output-file" (generate-png-filename tmx-path))))

(defun save-map-to-db! (connection tmx-path)
  (let* ((png-path (generate-png-filename tmx-path)))
    (generate-png tmx-path)
    (format t "Made png of ~a in ~a~%" tmx-path png-path)
    (cl-dbi:execute 
     (cl-dbi:prepare connection "INSERT INTO map (tmx_path, png_path) VALUES (?, ?)")
     (list tmx-path png-path))))
								       
    
