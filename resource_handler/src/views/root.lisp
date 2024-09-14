(defpackage linnarope.views.root
  (:use :cl)
  (:import-from :linnarope.middleware :@db :*connection* :@html :@css :deftab :defsubtab)
  (:import-from :easy-routes :defroute)
  (:import-from :lisp-fixup :with-output-to-real-string))

(in-package :linnarope.views.root)

(defroute css ("/css" :method :get :decorators (@css)) ()
  (cl-css:css `((body
		 :background-color "#fff")
		(.topbar
		 :display "flex"
		 :justify-content "space-evenly"
		 :list-style "none"
		 :flex-direction "row")
		(".topbar li"
		 :border-bottom "2px solid black")
		(".topbar li.selected"
		 :border-bottom "5px solid black")
		(label
		 :display "block")

		;; file browser css
		(.file-browser
		 :display "grid"
		 :grid-template-columns "repeat(3, 1fr)"
		 :gap "5px"
		 :height "fit-content")

		(.tmx
		 :background-color "#ff000033")
		 
		(".file-browser li"
		 :list-style "none")
		(".file-browser li:nth-child(odd)"
		 :grid-column "1")
		(".file-browser li:nth-child(even)"
		 :grid-column "2")
		(".file-browser li .dir"
		 :border-bottom "2px solid #3a00ff")

		(".file-browser li .file"
		 :border-bottom "2px solid #ff5700")


		;; current-map css
		(.map-container
		 :position "relative")
		(.warpzone
		 :position "absolute"))))

(deftab (maps "/maps" "maps.html") 
    (let ((maps (mapcar (lambda (row)
			  (let* ((id (getf row :id))
				 (tmx-path (getf row :|tmx_path|)))
			    `((:id . ,id)
			      (:name . ,tmx-path))))
			
			(cl-dbi:fetch-all
			 (cl-dbi:execute
			  (cl-dbi:prepare
			   *connection*
			   "SELECT * FROM map"))))))
      `((:maps . ,maps))))

(defun get-warpzone-objects (map-id)
  (assert *connection*)
  (mapcar
   (lambda (row)
     `((:name . ,(getf row :|name|))
       (:id . ,(getf row :|id|))
       (:x . ,(ceiling (getf row :|x|)))
       (:y . ,(ceiling (getf row :|y|)))))
   (cl-dbi:fetch-all
    (cl-dbi:execute
     (cl-dbi:prepare
      *connection*
      "SELECT * FROM object o JOIN objectgroup og ON og.id = o.group_id WHERE og.map_id = ? AND warp_zone = 1")
     (list map-id)))))

(defsubtab (current-map "/map/:id" "current_map.html" maps) ()
  (let ((warpzone-objects (get-warpzone-objects id)))
    `((:map-id . ,id)
      (:warpzone-objects . ,warpzone-objects))))

(defsubtab (new-map "/new-map" "new-map.html" maps) (&get path)
  (let ((path (or path (asdf:system-source-directory "linnarope-resource-handler"))))
    `((:path . ,path)
      (:files . ,(cons `((:file-path . ,(uiop:pathname-parent-directory-pathname path))
			 (:up . t)
			 (:dir . t))
		      (mapcar (lambda (f)
				`((:file-path . ,f)
				  (:tmx? . ,(equalp (pathname-type f) "tmx"))
				  (:dir . ,(cl-fad:directory-pathname-p f))))
			      (cl-fad:list-directory path)))))))

(defun filename (p)
  (format nil "~a.~a"
	  (pathname-name p)
	  (pathname-type p)))

(defsubtab (connect-warpzone-map-chooser "/connect-map/:src-map-id/:src-warpzone-id" "connect-warpzone-map-chooser.html" maps) ()
  (let ((maps (mapcar
	       (lambda (m)
		 `((:id . ,(getf m :|ID|))
		   (:name . ,(filename (getf m :|tmx_path|)))))
	       (cl-dbi:fetch-all (cl-dbi:execute (cl-dbi:prepare *connection*
								"SELECT id, tmx_path FROM map WHERE id <> ?")
						 (list src-map-id))))))
    `((:maps . ,maps)
      (:src-map-id . ,src-map-id)
      (:src-warpzone-id . ,src-warpzone-id))))

(defsubtab (dst-map-chooser "/open-connected-map-for-warpzone/:src-map-id/:src-warpzone-id" "dst-map-chooser.html" maps)
    (&get dst-map-id)
  (let ((warpzone-objects (get-warpzone-objects dst-map-id)))
    `((:src-map-id . ,src-map-id)
      (:src-warpzone-id . ,src-warpzone-id)
      (:dst-map-id . ,dst-map-id)
      (:warpzone-objects . ,warpzone-objects))))

(defroute root ("/" :method :get) ()
  (easy-routes:redirect 'maps))

(defroute map-add-handler ("/choose-map" :method :get :decorators (@db)) (&get tmx-file)
  (if tmx-file
      (progn 
	(linnarope.db.maps:save-map-to-db! *connection* tmx-file)
	(easy-routes:redirect 'maps))
      (progn
	(setf (hunchentoot:content-type*) "text/html")
	(setf (hunchentoot:return-code*) 400)
	"<p>tmx-files is nil</p>")))

(defroute map-spawnpoint-handler ("/select_destination"
				  ;; really illegal to fumble the db in a :get handler...
				  :method :get
				  :decorators (@db))
    (&get src-map-id src-warpzone-id dst-map-id dst-warpzone-id)
  (assert src-map-id)
  (assert src-warpzone-id)
  (assert dst-map-id)
  (assert dst-warpzone-id)
  (linnarope.db.maps:insert-warp-connection *connection* src-map-id src-warpzone-id dst-map-id dst-warpzone-id)
  (easy-routes:redirect 'maps))


(defroute map-img ("/map/:id/img" :method :get :decorators (@db)) ()
  (let* ((q (cl-dbi:prepare *connection* "SELECT png_path FROM map WHERE ID = ?"))
	 (rs (cl-dbi:fetch-all (cl-dbi:execute q (list id))))
	 (row (first rs))
	 (png-file-path (getf row :|png_path|))
	 (bytes (lisp-fixup:slurp-bytes png-file-path)))
    (setf (hunchentoot:content-type*) "image/png")
    bytes))
