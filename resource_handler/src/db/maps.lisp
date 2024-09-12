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
	  ((equalp expected-type :lisp-bool) (if (cdr v)
						 1
						 0))
	  ((equalp expected-type :double) (parse-number:parse-number (cdr v)))
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

(defun insert-objects-and-groups (connection map-id tmx-metadata)
  (let ((groups (get-alist tmx-metadata :object-groups)))
    (dolist (object-group groups)
      (let* ((group-id (get-alist object-group "id"))
	     (name (get-alist object-group "name")))
	(cl-dbi:execute 
	 (cl-dbi:prepare connection "INSERT INTO objectgroup VALUES (?, ?, ?)")
	 (list group-id name map-id))
	
	(dolist (obj (get-alist object-group :objects))
	  (let* ((id (get-alist obj "id"))
		 (name (or (get-alist obj "name") ""))
		 (x (get-alist obj "x" :expected-type :double))
		 (y (get-alist obj "y" :expected-type :double))
		 (width (get-alist obj "width" :expected-type :double))
		 (height (get-alist obj "height" :expected-type :double))
		 (warp-zone? (get-alist obj :warp-zone? :expected-type :lisp-bool)))
	    (assert name)
	    (cl-dbi:execute 
	     (cl-dbi:prepare connection "INSERT INTO object VALUES (?, ?, ?, ?, ?, ?, ?, ?)")
	     (list id name x y width height group-id warp-zone?))))))))
		 
	   
(defun save-map-to-db! (connection tmx-path)
  (let* ((png-path (generate-png-filename tmx-path))
	 (tmx-metadata (read-tmx tmx-path))
	 (map-id (insert-map-get-id connection tmx-path png-path tmx-metadata)))
    (insert-layers connection map-id tmx-metadata)
    (insert-objects-and-groups connection map-id tmx-metadata)    
    (generate-png tmx-path)
    (format t "Made png of ~a in ~a~%" tmx-path png-path)))


    
