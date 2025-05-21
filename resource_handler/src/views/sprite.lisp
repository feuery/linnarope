(defpackage linnarope.views.sprite
  (:use :cl)
  (:local-nicknames (:json :com.inuoe.jzon))
  (:export :sprites)
  (:import-from :lisp-fixup :filename)
  (:import-from :easy-routes :defroute)
  (:import-from :linnarope.middleware :@db :deftab :defsubtab))

(in-package :linnarope.views.sprite)


(deftab (sprites "/sprites" "sprites.html")
    `((:sprites . ,(postmodern:query "SELECT internal_id as \"id\", name FROM sprite" :alists))
      (:lisp-sprites . ,(postmodern:query
			 "SELECT id, name, w as width, h as height FROM lisp_sprite" :alists))
      (:palettes . ,(postmodern:query
		     "SELECT id, name FROM palette" :alists))))
				

(defsubtab (import-sprite "/import-sprite" "import-sprite.html" sprites) () (&get path)
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
	(linnarope.db.sprites:import-sprite! png-file)
	(easy-routes:redirect 'sprites))
      (progn
	(setf (hunchentoot:content-type*) "text/html")
	(setf (hunchentoot:return-code*) 400)
	"<p>png-files is nil</p>")))

(defroute sprite-img ("/sprite/:id" :method :get :decorators (@db)) ()
  (if (equalp id "")
      (progn
	(setf (hunchentoot:return-code*) 500)
	"")
      (let ((data (caar (postmodern:query "SELECT data FROM sprite WHERE internal_id = $1" id))))
	(setf (hunchentoot:content-type*) "image/png")
	data)))

(defroute delete-sprite ("/sprite/:id" :method :delete :decorators (@db)) ()
  (postmodern:execute "DELETE FROM sprite WHERE internal_id = $1" id)
  (setf (hunchentoot:return-code*) 204)
  "")

(defroute delete-lisp-sprite ("/lisp-sprite/:id" :method :delete :decorators (@db)) ()
  (postmodern:execute "DELETE FROM lisp_sprite WHERE id = $1" id)
  (setf (hunchentoot:return-code*) 204)
  "")

(defsubtab (edit-lisp-sprite "/edit-sprite/:id" "edit-sprite.html" sprites) () ()
  (let* ((data (coerce (postmodern:query
			"
SELECT *
FROM lisp_sprite spr 
JOIN palette pl ON spr.palette_id = pl.id
JOIN lisp_sprite_pixel pxl ON spr.id = pxl.sprite_id
WHERE spr.id = $1"  id :array-hash) 'list))
	 (colors-json (gethash "color_array" (first data)))
	 (palette-id (gethash "palette_id" (first data)))
	 (palettes (map 'list
			(lambda (r)
			  (assert (gethash "id" r))
			  (assert (gethash "name" r))
			  `((:palette-name . ,(gethash "name" r))
			    (:palette-id . ,(gethash "id" r))))
			(postmodern:query "SELECT ID, name FROM palette" :array-hash)))
	 (pixels (reduce (lambda (acc row)
			   (let ((x (gethash "x" row))
				 (y (gethash "y" row))
				 (color_index (gethash "color_index" row)))
			     (unless (gethash y acc)
			       (setf (gethash y acc) (make-hash-table :test 'equal)))
			     (setf (gethash x (gethash y acc)) color_index)
			     acc))
			 data :initial-value (make-hash-table :test 'equal))))
    (assert palette-id)
    `((:colors . ,colors-json)
      (:pixels . ,(json:stringify pixels))
      (:palettes . ,palettes)
      (:palette-id .,palette-id)
      (:id . ,id))))
