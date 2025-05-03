(defpackage linnarope.views.export
  (:use :cl)
  (:import-from :linnarope.middleware :@db :@html :@css :deftab :defsubtab)
  (:import-from :cl-hash-util :with-keys :hash)
  (:import-from :linnarope.migrations :*tables*)
  (:import-from :easy-routes :defroute)
  (:export :export-project)
  (:local-nicknames (:jzon :com.inuoe.jzon)))

(in-package :linnarope.views.export)

(deftab (export-things "/export-things" "export.html")
  nil)

(defsubtab (export-project-browser "/export-project" "export-project-browser.html" export-things) () (&get path)
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

(defun export-project (dst-path)
  (format t "Running `~a ~a`~%" *exporter-bin-path* dst-path)

  (sb-ext:run-program *exporter-bin-path* (list (format nil "~a" dst-path)) :output t)

  (format nil "Game package should now exist in ~a" dst-path))
  
(defroute export-project-here ("/export-project-here" :method :get
						      :decorators (@db))
    (&get path)
  (assert (cl-fad:file-exists-p *exporter-bin-path*))
  (let* ((zipfile-name "linnarope-export.game")
	 (final-path (pathname (format nil "~a/~a" path zipfile-name))))
    (export-project final-path)))

(defsubtab (import-project-browser "/import-project" "import-project-browser.html" export-things) () (&get path)
  (let ((path (or path (asdf:system-source-directory "linnarope-resource-handler"))))
    `((:path . ,path)
      (:files . ,(cons `((:file-path . ,(uiop:pathname-parent-directory-pathname path))
			 (:up . t)
			 (:dir . t))
		       (mapcar (lambda (f)
				 `((:file-path . ,f)
				   (:game? . ,(equalp (pathname-type f) "game"))
				   (:dir . ,(cl-fad:directory-pathname-p f))))
			       (cl-fad:list-directory path)))))))

(defroute import-this-project ("/import-this-project" :method :get
						      :decorators (@db))
    (&get game-file)
  (assert (cl-fad:file-exists-p *exporter-bin-path*))
  
  (dolist (table *tables*)
    (postmodern:execute (format nil "DELETE FROM ~a" table)))

  (sb-ext:run-program *exporter-bin-path* (list "-import" game-file))
  (format nil "Import successful!"))  
