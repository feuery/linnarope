(defpackage linnarope.db.maps
  (:use :cl)
  (:import-from :linnarope.tmx :read-tmx)
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

(defun get-alist (alist key &key (expected-type :string))
  (let ((v (assoc key alist :test 'equalp)))
    (cond ((equalp expected-type :int) (parse-integer (cdr v)))
	  ((equalp expected-type :sqlite-bool) (parse-integer (cdr v)))
	  (t (cdr v)))))

(defun insert-map-get-id (connection tmx-path png-path tmx-metadata)
  (getf (cl-dbi:fetch
	 (cl-dbi:execute 
	  (cl-dbi:prepare connection "INSERT INTO map

( tmx_path,
  png_path,
  orientation,
  renderorder,
  width,
  height,
  tilewidth,
  tileheight,
  infinite,
  nextlayerid,
  nextobjectid)

 VALUES
( ?,
  ?,
  ?,
  ?,
  ?,
  ?,
  ?,
  ?,
  ?,
  ?,
  ?) RETURNING ID")
	  (list tmx-path png-path
		(get-alist tmx-metadata "orientation")
		(get-alist tmx-metadata "renderorder")
		(get-alist tmx-metadata "width" :expected-type :int)
		(get-alist tmx-metadata "height" :expected-type :int)
		(get-alist tmx-metadata "tilewidth" :expected-type :int)
		(get-alist tmx-metadata "tileheight" :expected-type :int)
		(get-alist tmx-metadata "infinite" :expected-type :sqlite-bool)
		(get-alist tmx-metadata "nextlayerid" :expected-type :int)
		(get-alist tmx-metadata "nextobjectid" :expected-type :int))))
	:ID))

(defun insert-layers (connection map-id tmx-metadata)
  (let ((layers (get-alist tmx-metadata :layers)))
    (dolist (layer layers)
      (cl-dbi:execute 
       (cl-dbi:prepare connection "INSERT INTO layer
(ID, name, width, height, map_id)
VALUES
(?, ?, ?, ?, ?)")
       (list (get-alist layer "id" :expected-type :int)
	     (get-alist layer "name")
	     (get-alist layer "width" :expected-type :int)
	     (get-alist layer "height" :expected-type :int)
	     map-id)))))
	   
(defun save-map-to-db! (connection tmx-path)
  (let* ((png-path (generate-png-filename tmx-path))
	 (tmx-metadata (read-tmx tmx-path))
	 (map-id (insert-map-get-id connection tmx-path png-path tmx-metadata)))

    (insert-layers connection map-id tmx-metadata)
    
    (generate-png tmx-path)
    (format t "Made png of ~a in ~a~%" tmx-path png-path)))


    
