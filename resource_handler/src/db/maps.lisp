(defpackage linnarope.db.maps
  (:use :cl)
  (:import-from :linnarope.tmx :read-tmx)
  (:import-from :cl-hash-util :hash)
  (:import-from :linnarope.middleware :*database-name*)
  (:import-from :lisp-fixup :hashtable-merge)
  (:export :*whole-map-png-location* :generate-whole-map-png :get-object-internal-id :insert-warp-connection :save-map-to-db!))

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

(defvar *whole-map-png-location* (format nil "~awhole-map.resource-handler.png"
					 (uiop:pathname-parent-directory-pathname
					  (asdf:system-source-directory "linnarope-resource-handler"))))

(defun generate-whole-map-png ()
  "Generates the whole-map.png using engine and returns when it's done"
  (sb-ext:run-program *engine-binary-location*
		      (list "--png-output-file" *whole-map-png-location*
			    "--whole-map" *database-name*)))

(defun get-alist (alist key &key (expected-type :string))
  (let ((v (assoc key alist :test 'equalp)))
    (cond ((equalp expected-type :int) (handler-bind ((sb-int:simple-parse-error
							(lambda (c)
							  (format t "got error ~a~%" c)
							  (return-from get-alist 
							    (round 
							     (parse-number:parse-number (cdr v)))))))
					 (parse-integer (cdr v))))
	  ((equalp expected-type :sqlite-bool) (parse-integer (cdr v)))
	  ((equalp expected-type :lisp-bool) (if (cdr v)
						 1
						 0))
	  ((equalp expected-type :double) (parse-number:parse-number (cdr v)))
	  (t (cdr v)))))

(defun insert-map-get-id (tmx-path png-path tmx-metadata)
  (caar (postmodern:query
	 "INSERT INTO map

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
  nextobjectid,
  tmx_file)

 VALUES
( $1,
  $2,
  $3,
  $4,
  $5,
  $6,
  $7,
  $8,
  $9,
  $10,
  $11,
  $12) RETURNING ID"
	 (lisp-fixup:filename tmx-path)
	 png-path
	 (get-alist tmx-metadata "orientation")
	 (get-alist tmx-metadata "renderorder")
	 (get-alist tmx-metadata "width" :expected-type :int)
	 (get-alist tmx-metadata "height" :expected-type :int)
	 (get-alist tmx-metadata "tilewidth" :expected-type :int)
	 (get-alist tmx-metadata "tileheight" :expected-type :int)
	 (get-alist tmx-metadata "infinite" :expected-type :sqlite-bool)
	 (get-alist tmx-metadata "nextlayerid" :expected-type :int)
	 (get-alist tmx-metadata "nextobjectid" :expected-type :int)
	 (lisp-fixup:slurp-bytes tmx-path))))

(defun insert-layers ( map-id tmx-metadata)
  "Inserts map's :layers into the db and returns a list of maps of {layerid -> internal-id}"
  (let* ((layers (get-alist tmx-metadata :layers)))
    (reduce #'hashtable-merge 
	    (mapcar (lambda (layer)
		      (destructuring-bind (id internal-id) (first (postmodern:query
								   "INSERT INTO layer
(ID, name, width, height, map_id)
VALUES
($1, $2, $3, $4, $5) RETURNING id, internal_id"
								   (get-alist layer "id" :expected-type :int)
								   (get-alist layer "name")
								   (get-alist layer "width" :expected-type :int)
								   (get-alist layer "height" :expected-type :int)
								   map-id))
			(hash (id internal-id))))
		    layers))))

(defun insert-objects-and-groups ( map-id tmx-metadata)
  "Inserts objects and groups to db and returns `(values group-mapping object-mapping)`, which map tmx-local ids to sqlite internal auto_increment ids"
  (let* ((groups (get-alist tmx-metadata :object-groups))
	 (group-mapping (reduce #'hashtable-merge
				(mapcar (lambda (group)
					  (let ((group-id (get-alist group "id"))
						(name (get-alist group "name")))
					    (destructuring-bind (objectgroup-id objectgroup-internal-id)
						(car (postmodern:query "INSERT INTO objectgroup (ID, name, map_id) VALUES ($1, $2, $3) returning id, internal_id"
								  group-id name map-id))

					      (hash (objectgroup-id objectgroup-internal-id)))))
					groups)))
	 (objs (mapcan (lambda (ogroup)
			 (mapcar (lambda (obj)
				   (let ((internal-id (gethash (parse-number:parse-number (get-alist ogroup "id")) group-mapping)))
				     (assert internal-id)
				     (cons (cons :group-internal-id internal-id) obj)))
				 (get-alist ogroup :objects)))
		       groups)))
    (let* ((object-mapping (reduce #'hashtable-merge
				   (mapcar (lambda (obj)
					     (let* ((id (get-alist obj "id"))
						    (name (or (get-alist obj "name") ""))
						    (x (get-alist obj "x" :expected-type :int))
						    (y (get-alist obj "y" :expected-type :int))
						    (width (get-alist obj "width" :expected-type :int))
						    (height (get-alist obj "height" :expected-type :int))
						    (warp-zone? (get-alist obj :warp-zone? :expected-type :lisp-bool))
						    (group-id (get-alist obj :group-internal-id)))
					       (assert group-id)
					       (destructuring-bind (id internal-id) (first
										     (postmodern:query
										      "INSERT INTO object (id, name, x, y, width, height, group_id, warp_zone) VALUES ($1, $2, $3, $4, $5, $6, $7, $8) RETURNING id, internal_id" id name x y width height group-id warp-zone?))
						 (hash (id internal-id)))))
					   objs))))

      (values group-mapping object-mapping))))

(defun insert-tilesets (tmx-metadata map-id)
  (let ((tsets (get-alist tmx-metadata :tilesets)))
    (dolist (tileset-path tsets)
      (let ((filename (lisp-fixup:filename tileset-path))
	    (bytes (lisp-fixup:slurp-bytes tileset-path)))
	(postmodern:execute "INSERT INTO tileset (filename, tsx_contents) VALUES ($1, $2) ON CONFLICT DO NOTHING" filename bytes)
	(postmodern:execute "INSERT INTO map_to_tileset (map_id, tileset_filename) VALUES ($1, $2)" map-id filename)))))
	   
(defun save-map-to-db! ( tmx-path)
  (let* ((png-path (generate-png-filename tmx-path))
	 (tmx-metadata (read-tmx tmx-path))
	 (map-id (insert-map-get-id  tmx-path png-path tmx-metadata))
	 (layer-mapping (insert-layers  map-id tmx-metadata)))
    (insert-tilesets tmx-metadata map-id)
    (multiple-value-bind (group-mapping object-mapping) (insert-objects-and-groups  map-id tmx-metadata)
      (generate-png tmx-path)
      (format t "Inserted ~d layers, ~d groups and ~d objects ~%"
	      (hash-table-count layer-mapping)
	      (hash-table-count group-mapping)
	      (hash-table-count object-mapping))
      
      (format t "Made png of ~a in ~a~%" tmx-path png-path))))

(defun get-object-internal-id ( id map-id)
  (caar (postmodern:query "
SELECT o.internal_id
FROM object o
JOIN objectgroup ogroup ON o.group_id = ogroup.ID 
WHERE o.id = $1 and ogroup.map_id = $2" id map-id)))

(defun insert-warp-connection (src-map-id src-warpzone-id dst-map-id dst-warpzone-id)
  (postmodern:execute "INSERT INTO warp_connection (src_map, src_warpzone, dst_map, dst_warpzone) VALUES ($1, $2, $3, $4)"
		      src-map-id
		      (get-object-internal-id  src-warpzone-id src-map-id)
		      dst-map-id
		      (get-object-internal-id  dst-warpzone-id dst-map-id)))
