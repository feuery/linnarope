(defpackage linnarope.routes.sprite
  (:import-from :easy-routes :defroute :redirect)
  (:import-from :linnarope.middleware :@db )
  (:import-from :linnarope.views.root :read-arrayed-form)
  (:import-from :cl-hash-util :with-keys)
  (:use :cl)
  (:local-nicknames (:json :com.inuoe.jzon)))

(in-package :linnarope.routes.sprite)

(defroute lispsprite-saver ("/new-lisp-sprite" :method :post :decorators (@db)) ()
  (with-keys ("name" "w" "h" "palette_id") (read-arrayed-form)
    (let* ((ww (parse-integer w))
	   (hh (parse-integer h))

	   (pixels (let ((acc))
		     (dotimes (x ww)
		       (push (lisp-fixup:range hh) acc))
		     acc))

	   (row (aref (postmodern:query "INSERT INTO lisp_sprite (name, w, h, palette_id, pixels) VALUES ($1, $2, $3, $4, $5) returning id" 
					name w h palette_id (json:stringify pixels) :array-hash) 0))
	   (sprite-id (gethash "id" row)))
      (assert sprite-id)
      (redirect 'linnarope.views.sprite:sprites))))

(defroute lisp-sprite-editor ("/save-lisp-sprite/:id" :method :post :decorators (@db)) ()
  (postmodern:execute "UPDATE lisp_sprite SET pixels = $1 WHERE id = $2" (hunchentoot:raw-post-data :force-text t) id)
  (setf (hunchentoot:return-code*) 204)
  "")

(defroute palette-changer ("/sprite/:id/change-palette" :method :post :decorators (@db)) ()
  (let* ((body (json:parse (hunchentoot:raw-post-data :force-text t)))
	 (palette-id (gethash "palette-id" body)))
    (assert palette-id)
    (postmodern:execute "
UPDATE lisp_sprite
SET palette_id = $1
WHERE ID = $2" palette-id id)

    (let ((row (coerce (postmodern:query "SELECT color_array FROM palette WHERE ID = $1" palette-id :array-hash) 'list)))
      (assert row)
      
      (gethash "color_array" (first row)))))
  
    
