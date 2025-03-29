(defpackage linnarope.views.export
  (:use :cl)
  (:import-from :linnarope.middleware :@db :@html :@css :deftab :defsubtab)
  (:import-from :cl-hash-util :with-keys :hash)
  (:import-from :easy-routes :defroute)
  (:local-nicknames (:jzon :com.inuoe.jzon)))

(in-package :linnarope.views.export)

(deftab (export-things "/export-things" "export.html")
  nil)

(defvar header-folder (asdf:system-relative-pathname "linnarope-resource-handler" "../generated-include"))

(defparameter header-template 
  "
#pragma once
/* sprites */
~{int ~a = ~d;~%~}
/* TODO maps & others */
")

(defroute do-exporting ("/export" :method :post :decorators (@db)) ()
  (format t "Generating C headers in ~a~%" header-folder)
  (let* ((db-lisp-sprites (postmodern:query "SELECT ID, name FROM lisp_sprite"))
	 (db-regular-sprites nil)
	 (db-sprites (concatenate 'list db-lisp-sprites db-regular-sprites))
	 (sprites (reduce 
		   (lambda (acc row)
		     (destructuring-bind (id name) row
		       (assert id)
		       (assert name)
		       (cons name (cons id acc))))
		   db-sprites
		   :initial-value nil))
	 (header-code (format nil header-template sprites)))
    (format t "Header code ~a~%" header-code)
    header-code))

(defsubtab (export-project-browser "/export-project" "export-project-browser.html" do-exporting) () (&get path)
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

(defun gen2d (h w)
  (coerce 
   (loop for y from 1 to  h
	 collecting (coerce (loop 
			      for x from 1 to w
			      collecting 0)
			    'vector))
   'vector))



  

(defroute export-project-here ("/export-project-here" :method :get
						      :decorators (@db))
    (&get path)
  (let* ((zipfile-name "linnarope-export.game")
	 ;; TODO don't commit :D
	 ;;(path #P"/Users/feuer/Projects/finrope/resource_handler/src/views")
	 (final-path (pathname (format nil "~a/~a" path zipfile-name)))
	 (temp-dir (pathname (format nil "~a/linnarope.tmp/" path)))
	 (map-dir (pathname (format nil "~a/maps/" temp-dir)))
	 (sprite-dir (pathname (format nil "~a/sprites/" temp-dir)))
	 (lisp-sprite-dir (pathname (format nil "~a/lisp-sprites/" temp-dir)))
	 (palettes-dir (pathname (format nil "~a/palettes/" temp-dir)))
	 (warp-connections-dir (pathname (format nil "~a/warp-connections/" temp-dir)))
	 

	 (map-blobs (coerce (postmodern:query "SELECT tmx_path, tmx_file FROM map" :array-hash) 'list))
	 (sprites (coerce (postmodern:query "SELECT name, data FROM sprite" :array-hash) 'list))


	 (palettes (map 'list
			(lambda (r)
			  (destructuring-bind (name colors) r
			    (hash ("name" name)
				  ("colors" (jzon:parse colors)))))
			(postmodern:query "SELECT name, color_array FROM palette")))
	 (lisp-sprites (map 'list
			    (lambda (sprite)
			      (with-keys ("id" "w" "h") sprite
				(let* ((pixel-dst (gen2d h w))
				       (pixel-data (postmodern:query "SELECT x, y, color_index FROM lisp_sprite_pixel WHERE sprite_id = $1" id))
				       ;; (pixls (pixels-to-2d pixel-data))
				       )
				  (dolist (pixel-row pixel-data)
				    (destructuring-bind (x y color-index) pixel-row
					(setf (aref (aref pixel-dst y) x) color-index)))
				  (format t "pixels: ~a~%" pixel-dst)
				  
				  (setf (gethash "pixels" sprite) pixel-dst)
				  sprite)))
				  
			    (postmodern:query "SELECT * FROM lisp_sprite" :array-hash)))

	 (warp-connections (coerce (postmodern:query "SELECT * FROM warp_connection" :array-hash) 'list))
	 (all-dirs (list temp-dir map-dir sprite-dir lisp-sprite-dir palettes-dir warp-connections-dir)))
				  
    
    (dolist (dir all-dirs)
      (ensure-directories-exist dir))

    (dolist (map-row map-blobs)
      (with-keys ("tmx_path" "tmx_file") map-row 
	(with-open-file (f (pathname (format nil "~a/~a" map-dir tmx_path)) :if-exists :overwrite
									    :element-type '(unsigned-byte 8)
									    :direction :output
									    :if-does-not-exist :create)
	  (write-sequence tmx_file f))))

    (dolist (sprite-row sprites)
      (with-keys ("name" "data") sprite-row 
	(with-open-file (f (pathname (format nil "~a/~a" sprite-dir name)) :if-exists :overwrite
									    :element-type '(unsigned-byte 8)
									    :direction :output
									    :if-does-not-exist :create)
	  (write-sequence data f))))

    (dolist (lisp-sprite lisp-sprites)
      (with-keys ("name") lisp-sprite
	(assert name)
	(with-open-file (f (pathname (format nil "~a/~a.json" lisp-sprite-dir name))
			   :if-exists :overwrite
			   :direction :output
			   :if-does-not-exist :create)
	  (write-string (jzon:stringify lisp-sprite) f))))

    (dolist (palette palettes)
      (with-keys ("name") palette
	(assert name)
	(with-open-file (f (pathname (format nil "~a/~a.json" palettes-dir name))
			   :if-exists :overwrite
			   :direction :output
			   :if-does-not-exist :create)
	  (write-string (jzon:stringify palette) f))))

    (dolist (wc warp-connections)
      (with-keys ("internal_id") wc
	(assert internal_id)
	(with-open-file (f (pathname (format nil "~a/~a.json" warp-connections-dir internal_id))
			   :if-exists :overwrite
			   :direction :output
			   :if-does-not-exist :create)
	  (write-string (jzon:stringify wc) f))))

    (let ((files (lisp-fixup:recursive-dir-files temp-dir)))
      (format t "Files: ~a~%" files))

    (format t "Removing ~a recursively~%" temp-dir)
    ;;(cl-fad:delete-directory-and-files temp-dir)

    ;; sen jälkeen zippaa temp-dir $path/$zipfile-name:en ja poista temp-dir 

    (format nil "Files should now exist in ~a" temp-dir)))
