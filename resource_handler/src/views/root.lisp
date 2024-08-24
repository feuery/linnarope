(defpackage linnarope.views.root
  (:use :cl)
  (:import-from :linnarope.middleware :@db :*connection* :@html :@css :deftab)
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
		 :display "block"))))

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

(deftab (sprites "/sprites" "sprites.html")
    '((:sprite-data . "lolz")))

(defroute root ("/" :method :get) ()
  (easy-routes:redirect 'maps))


(defroute new-map-handler ("/insert-map" :method :post :decorators (@db)) (&post new-map-path)
  (destructuring-bind (tmp-file filename mime) new-map-path
    (let ((file-blob (lisp-fixup:slurp-utf-8 tmp-file)))
      (cl-dbi:execute (cl-dbi:prepare *connection* "INSERT INTO map (name, file_data) VALUES (?,?)") (list filename file-blob)) 
      (easy-routes:redirect 'maps))))
