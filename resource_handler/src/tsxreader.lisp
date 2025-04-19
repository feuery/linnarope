(defpackage linnarope.tsx
  (:use :cl)
  (:import-from :cl-hash-util :hash)
  (:import-from :lisp-fixup :slurp-utf-8 :partial :compose)
  (:import-from :linnarope.tmx :alistify-attrs)
  (:export :read-tsx)
  (:documentation "A lisp port of the tmxreader.cpp found somewhere in this repo"))

(in-package :linnarope.tsx)

(defun read-tsx (xml)
  "Currently reads only <image> tags from tsx xml"
  (let* ((doc (xmls:parse xml))
	 (kids (xmls:node-children doc)))
    (mapcar
     (lambda (el)
       (let* ((attrs (alistify-attrs (xmls:node-attrs el)))
	      (width (cdr (assoc "width" attrs :test 'equal)))
	      (height (cdr (assoc "height" attrs :test 'equal)))
	      (source (cdr (assoc "source" attrs :test 'equal))))
	 (hash ("width" width)
	       ("height" height)
	       ("source" source))))
			
     (remove-if-not (lambda (node)
		      (string= (xmls:node-name node) "image"))
		    kids))))

;; (read-tsx (lisp-fixup:slurp-utf-8
;; 	   #P"/Users/feuer/Projects/finrope/maps/terrain.tsx"))
