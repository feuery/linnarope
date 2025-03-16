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
    (let* ((row (aref (postmodern:query "INSERT INTO lisp_sprite (name, w, h, palette_id) VALUES ($1, $2, $3, $4) returning id" 
					name w h palette_id :array-hash) 0))
	   (sprite-id (gethash "id" row)))
      (assert sprite-id)
      (let ((ww (parse-integer w))
	    (hh (parse-integer h)))
	(dotimes (x ww)
	  (dotimes (y hh)
	    (postmodern:execute
	     "INSERT INTO lisp_sprite_pixel (sprite_id, x, y ) VALUES ($1, $2, $3)"
	     sprite-id x y))))
      (redirect 'linnarope.views.sprite:sprites))))

(defroute lisp-sprite-editor ("/save-lisp-sprite/:id" :method :post :decorators (@db)) ()
  (let* ((body (hunchentoot:raw-post-data :force-text t))
	 (res (aref (postmodern:query "SELECT w, h FROM lisp_sprite WHERE id = $1" id :array-hash) 0))
	 (w (gethash "w" res))
	 (h (gethash "h" res))
	 (pixels (json:parse body)))
    
    (assert (and w h))
    (dotimes (x w)
      (dotimes (y h)
	(let ((pix (aref (aref pixels y) x)))
	  (postmodern:execute "UPDATE lisp_sprite_pixel SET color_index = $1 WHERE sprite_id = $2 AND x = $3 AND y = $4"
			      pix id x y))))
    
    (setf (hunchentoot:return-code*) 204)
    ""))

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
  
    
