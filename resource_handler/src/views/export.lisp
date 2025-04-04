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

(defvar *exporter-bin-path* (pathname (format nil "~aexporter/exporter" (asdf:system-source-directory "linnarope-resource-handler"))))

(defroute export-project-here ("/export-project-here" :method :get
						      :decorators (@db))
    (&get path)
  (assert (cl-fad:file-exists-p *exporter-bin-path*))
  (let* ((zipfile-name "linnarope-export.game")
	 (final-path (pathname (format nil "~a/~a" path zipfile-name))))

    (format t "Running `~a ~a`~%" *exporter-bin-path* final-path)

    (sb-ext:run-program *exporter-bin-path* (list (format nil "~a" final-path)))

    (format nil "Game package should now exist in ~a" final-path)))
