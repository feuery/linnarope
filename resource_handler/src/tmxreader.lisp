(defpackage linnarope.tmx
  (:use :cl)
  (:import-from :lisp-fixup :slurp-utf-8 :partial :compose)
  (:export :read-tmx :alistify-attrs)
  (:documentation "A lisp port of the tmxreader.cpp found somewhere in this repo"))

(in-package :linnarope.tmx)

(defun alistify-attrs (attrs)
  (reduce (lambda (acc a)
	    (cons 
	     (cons (first a) (second a))
	     acc))
	  attrs :initial-value nil))

(defun parse-objectgroups (tmx-children)
  (mapcar (lambda (ogroup)
	    (let ((attrs (alistify-attrs (xmls:node-attrs ogroup)))
		  (objects (xmls:node-children ogroup)))
	      (cons (cons :objects (mapcar
				    (lambda (object)
				      (let ((attrs (alistify-attrs (xmls:node-attrs object)))
					    (warp-zone? (some (lambda (child)
								(string= "ellipse" (xmls:node-name child)))
							      (xmls:node-children object))))
					(cons (cons :warp-zone? warp-zone?)
					      attrs)))
					
				    objects))
		    attrs)))
	  
	  (remove-if-not (compose (partial #'string= "objectgroup")
				  #'xmls:node-name)
			 tmx-children)))

(defun read-tmx (tmx-path)
  (let* ((containing-dir (funcall (compose #'pathname #'directory-namestring) tmx-path))
	 (xml-data (slurp-utf-8 tmx-path))
	 (doc (xmls:parse xml-data))
	 (map-attrs (xmls:node-attrs doc))
	 (kids (xmls:node-children doc))
	 (tilesets (mapcar (compose
			    #'pathname
			    (lambda (source)
			      (format nil "~a~a" containing-dir source))
			    (lambda (attr-list)
			      (cadar
			       (remove-if-not (compose (partial #'string= "source") #'first) attr-list)))
			    #'xmls:node-attrs)
			   (remove-if-not (lambda (node)
					    (string= (xmls:node-name node) "tileset"))
					  kids)))
	 (layers (mapcar (compose
			  #'alistify-attrs
			  #'xmls:node-attrs)
			 (remove-if-not (lambda (node)
				  (string= (xmls:node-name node) "layer"))
					kids)))
	 (object-groups (parse-objectgroups kids)))
    (concatenate 'list
		 (list (cons :tilesets tilesets)
		       (cons :layers layers)
		       (cons :object-groups object-groups))
		 (alistify-attrs map-attrs))))

;; (read-tmx #P"/Users/feuer/Projects/finrope/maps/pikkustadi-töölön tulli.tmx")
