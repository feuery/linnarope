(defpackage linnarope.routes.sprite
  (:import-from :easy-routes :defroute :redirect)
  (:import-from :linnarope.middleware :@db :*connection*)
  (:import-from :linnarope.views.root :read-arrayed-form)
  (:import-from :cl-hash-util :with-keys)
  (:use :cl)
  (:local-nicknames (:json :com.inuoe.jzon)))

(in-package :linnarope.routes.sprite)

(defroute lispsprite-saver ("/new-lisp-sprite" :method :post :decorators (@db)) ()
  (with-keys ("name" "w" "h" "palette_id") (read-arrayed-form)
    (let* ((row (cl-dbi:fetch
		 (cl-dbi:execute (cl-dbi:prepare *connection* "INSERT INTO lisp_sprite (name, w, h, palette_id) VALUES (?, ?, ?, ?) returning id")
				 (list name w h palette_id))))
	   (sprite-id (getf row :ID)))
      (assert sprite-id)
      (let ((ww (parse-integer w))
	    (hh (parse-integer h)))
      (dotimes (x ww)
	(dotimes (y hh)
	  (cl-dbi:execute (cl-dbi:prepare *connection* "INSERT INTO lisp_sprite_pixel (sprite_id, x, y ) VALUES (?, ?, ?)")
			  (list sprite-id x y)))))
      (redirect 'linnarope.views.sprite:sprites))))

(defroute lisp-sprite-editor ("/save-lisp-sprite/:id" :method :post :decorators (@db)) ()
  (let* ((body (hunchentoot:raw-post-data :force-text t))
	 (res (cl-dbi:fetch (cl-dbi:execute (cl-dbi:prepare *connection* "SELECT w, h FROM lisp_sprite WHERE id = ?") (list id))))
	 (w (getf res :|w|))
	 (h (getf res :|h|))
	 (pixels (json:parse body))

	 (update-pixel-stmt (cl-dbi:prepare *connection* "UPDATE lisp_sprite_pixel SET color_index = ? WHERE sprite_id = ? AND x = ? AND y = ?")))
    
    (assert (and w h))
    (dotimes (x w)
      (dotimes (y h)
	(let ((pix (aref (aref pixels y) x)))
	  (cl-dbi:execute update-pixel-stmt (list pix id x y)))))
    
    (setf (hunchentoot:return-code*) 204)
    ""))

(defroute palette-changer ("/sprite/:id/change-palette" :method :post :decorators (@db)) ()
  (let* ((body (json:parse (hunchentoot:raw-post-data :force-text t)))
	 (palette-id (gethash "palette-id" body)))
    (assert palette-id)
    (cl-dbi:execute
     (cl-dbi:prepare *connection*
		     "
UPDATE lisp_sprite
SET palette_id = ?
WHERE ID = ?")
     (list palette-id id))

    (let ((palette (getf (cl-dbi:fetch
			  (cl-dbi:execute
			   (cl-dbi:prepare
			    *connection*
			    "SELECT color_array FROM palette WHERE ID = ?")
			   (list palette-id)))
			 :|color_array|)))
      palette)))
  
    
