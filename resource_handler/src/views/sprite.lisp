(defpackage linnarope.views.sprite
  (:use :cl)
  (:import-from :lisp-fixup :filename)
  (:import-from :easy-routes :defroute)
  (:import-from :linnarope.middleware :@db :*connection* :deftab :defsubtab))

(in-package :linnarope.views.sprite)
     

(deftab (sprites "/sprites" "sprites.html")
    `((:sprites . ,(mapcar (lambda (sprite)
			     `((:id . ,(getf sprite :|internal_id|))
			       (:name . ,(filename (getf sprite :|png_path|)))))

			   (cl-dbi:fetch-all
			    (cl-dbi:execute
			     (cl-dbi:prepare
			      *connection*
			      "SELECT * FROM sprite")))))))

(defsubtab (import-sprite "/import-sprite" "import-sprite.html" sprites) (&get path)
  (let ((path (or path (asdf:system-source-directory "linnarope-resource-handler"))))
    `((:path . ,path)
      (:files . ,(cons `((:file-path . ,(uiop:pathname-parent-directory-pathname path))
			 (:up . t)
			 (:dir . t))
		      (mapcar (lambda (f)
				`((:file-path . ,f)
				  (:png? . ,(equalp (pathname-type f) "png"))
				  (:dir . ,(cl-fad:directory-pathname-p f))))
			      (cl-fad:list-directory path)))))))

(defroute sprite-import-handler ("/choose-sprite" :method :get :decorators (@db)) (&get png-file)
  (if png-file
      (progn 
	(linnarope.db.sprites:import-sprite! *connection* png-file)
	(easy-routes:redirect 'sprites))
      (progn
	(setf (hunchentoot:content-type*) "text/html")
	(setf (hunchentoot:return-code*) 400)
	"<p>png-files is nil</p>")))

(defroute sprite-img ("/sprite/:id" :method :get :decorators (@db)) ()
  (let* ((q (cl-dbi:prepare *connection* "SELECT png_path FROM sprite WHERE internal_id = ?"))
	 (rs (cl-dbi:fetch-all (cl-dbi:execute q (list id))))
	 (row (first rs))
	 (png-file-path (getf row :|png_path|))
	 (bytes (lisp-fixup:slurp-bytes png-file-path)))
    (setf (hunchentoot:content-type*) "image/png")
    bytes))

(defroute delete-sprite ("/sprite/:id" :method :delete :decorators (@db)) ()
  (cl-dbi:execute (cl-dbi:prepare *connection*
				  "DELETE FROM sprite WHERE internal_id = ?")
		  (list id))
  (setf (hunchentoot:return-code*) 204)
  "")
