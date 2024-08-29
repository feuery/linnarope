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

		(".file-browser li"
		 :list-style "none")
		(".file-browser li:nth-child(odd)"
		 :grid-column "1")
		(".file-browser li:nth-child(even)"
		 :grid-column "2")
		(".file-browser li .dir"
		 :border-bottom "2px solid #3a00ff")

		(".file-browser li .file"
		 :border-bottom "2px solid #ff5700"))))

(deftab (maps "/maps" "maps.html") 
    (let ((maps (mapcar (lambda (row)
			  (let* ((id (getf row :id))
				 (filename (getf row :|name|)))
			    `((:id . ,id)
			      (:name . ,filename))))
			
			(cl-dbi:fetch-all
			 (cl-dbi:execute
			  (cl-dbi:prepare
			   *connection*
			   "SELECT * FROM map"))))))
      `((:maps . ,maps))))

(defsubtab (current-map "/map/:id" "current_map.html" maps) ()
  `((:map-id . ,id)))

(defsubtab (new-map "/new-map" "new-map.html" maps) (&get path)
  (let ((path (or path (asdf:system-source-directory "linnarope-resource-handler"))))
    `((:path . ,path)
      (:files . ,(cons `((:file-path . ,(uiop:pathname-parent-directory-pathname path))
			 (:up . t)
			 (:dir . t))
		      (mapcar (lambda (f)
				`((:file-path . ,f)
				  (:dir . ,(cl-fad:directory-pathname-p f))))
			      (cl-fad:list-directory path)))))))

(defroute root ("/" :method :get) ()
  (easy-routes:redirect 'maps))

(defroute new-map-handler ("/add-map-path" :method :get :decorators (@db)) (&get path)
  (let* ((files (cl-fad:list-directory path)))
    (setf (hunchentoot:content-type*) "text/html")
    (if (and (some (lisp-fixup:compose (lisp-fixup:partial #'string= "tmx") #'pathname-type) files))
	"<p>ONNEA TMX LÖYTYI</p>"
	"<p>tmx ei löytynyt :/</p>")))
