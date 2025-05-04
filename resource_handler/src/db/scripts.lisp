(defpackage linnarope.db.scripts
  (:use :cl)
  (:export :all-scripts))

(in-package :linnarope.db.scripts)

(defun all-scripts ()
  (postmodern:query "SELECT ID, NAME FROM script" :alists))
  
