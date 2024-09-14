(defpackage linnarope.db.maps
  (:use :cl)
  (:import-from :linnarope.tmx :read-tmx)
  (:import-from :lisp-fixup :hashtable-merge)
  (:export :insert-warp-connection :save-map-to-db!))

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
  "Inserts map's :layers into the db and returns a list of maps of {layerid -> internal-id}"
  (let* ((layers (get-alist tmx-metadata :layers)))
    (reduce #'hashtable-merge 
	    (mapcar (lambda (layer)
		      (let ((id-map (make-hash-table :test 'equal :size 2))
			    (result (cl-dbi:fetch
				     (cl-dbi:execute 
				      (cl-dbi:prepare connection "INSERT INTO layer
(ID, name, width, height, map_id)
VALUES
(?, ?, ?, ?, ?) RETURNING id, internal_id")
				      (list (get-alist layer "id" :expected-type :int)
					    (get-alist layer "name")
					    (get-alist layer "width" :expected-type :int)
					    (get-alist layer "height" :expected-type :int)
					    map-id)))))
			(setf (gethash (getf result "id") id-map)
			      (getf result "internal_id"))
			id-map))
		    layers))))

(defun insert-objects-and-groups (connection map-id tmx-metadata)
  "Inserts objects and groups to db and returns `(values group-mapping object-mapping)`, which map tmx-local ids to sqlite internal auto_increment ids"
  (let* ((groups (get-alist tmx-metadata :object-groups))
	 (group-mapping (reduce #'hashtable-merge
				(mapcar (lambda (group)
					  (let* ((group-id (get-alist group "id"))
						 (name (get-alist group "name"))
						 (group-result (cl-dbi:fetch
								(cl-dbi:execute 
								 (cl-dbi:prepare connection "INSERT INTO objectgroup (ID, name, map_id) VALUES (?, ?, ?) returning id, internal_id")
								 (list group-id name map-id))))
						 (id-map (make-hash-table :test 'equal :size 2)))
					    (setf (gethash (getf group-result :ID) id-map)
						  (getf group-result :|internal_id|))
					    id-map))
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
						    (x (get-alist obj "x" :expected-type :double))
						    (y (get-alist obj "y" :expected-type :double))
						    (width (get-alist obj "width" :expected-type :double))
						    (height (get-alist obj "height" :expected-type :double))
						    (warp-zone? (get-alist obj :warp-zone? :expected-type :lisp-bool))
						    (group-id (get-alist obj :group-internal-id))
						    (_ (assert group-id))
						    (obj-result (cl-dbi:fetch 
								 (cl-dbi:execute 
								  (cl-dbi:prepare connection "INSERT INTO object (id, name, x, y, width, height, group_id, warp_zone) VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING id, internal_id")
								  (list id name x y width height group-id warp-zone?))))
						    (id-map (make-hash-table :test 'equal :size 2)))
					       (setf (gethash (getf obj-result "id") id-map)
						     (getf obj-result "internal_id"))
					       id-map))
					   objs))))

      (values group-mapping object-mapping))))
		 
	   
(defun save-map-to-db! (connection tmx-path)
  (let* ((png-path (generate-png-filename tmx-path))
	 (tmx-metadata (read-tmx tmx-path))
	 (map-id (insert-map-get-id connection tmx-path png-path tmx-metadata))
	 (layer-mapping (insert-layers connection map-id tmx-metadata)))
    (multiple-value-bind (group-mapping object-mapping) (insert-objects-and-groups connection map-id tmx-metadata)
      (generate-png tmx-path)
      (format t "Inserted ~d layers, ~d groups and ~d objects ~%"
	      (hash-table-count layer-mapping)
	      (hash-table-count group-mapping)
	      (hash-table-count object-mapping))
      
      (format t "Made png of ~a in ~a~%" tmx-path png-path))))

(defun get-object-internal-id (connection id map-id)
  (getf (cl-dbi:fetch
	 (cl-dbi:execute
	  (cl-dbi:prepare connection "
SELECT o.internal_id
FROM object o
JOIN objectgroup ogroup ON o.group_id = ogroup.ID 
WHERE o.id = ? and ogroup.map_id = ?")
	  (list id map-id)))
	:|internal_id|))

(defun insert-warp-connection (connection src-map-id src-warpzone-id dst-map-id dst-warpzone-id)
  (cl-dbi:execute
   (cl-dbi:prepare connection "INSERT INTO warp_connection (src_map, src_warpzone, dst_map, dst_warpzone) VALUES (?, ?, ?, ?)")
   (list src-map-id (get-object-internal-id connection src-warpzone-id src-map-id)
	 dst-map-id (get-object-internal-id connection dst-warpzone-id dst-map-id))))
