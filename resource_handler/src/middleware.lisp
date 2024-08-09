(defpackage linnarope.middleware
  (:use :cl)
  (:import-from :lisp-fixup :with-output-to-real-string)
  (:export :restored-stdout :@html))

(in-package :linnarope.middleware)

(defvar *stdout* nil)

(defun doctype (body)
  (format nil "<!doctype html>~%~a" body))

(defun @html (next)
  (let ((*stdout* *standard-output*))
    (doctype
     (with-output-to-real-string
	 (funcall next)))))

(defmacro restored-stdout (&rest body)
  "Restores (format nil) to output to stdout inside a @html handler"
  `(let ((*standard-output* *stdout*))
     ,@body))
			    
