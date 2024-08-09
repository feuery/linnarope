(defpackage linnarope.views.root
  (:use :cl)
  (:import-from :linnarope.middleware :@html :restored-stdout)
  (:import-from :macro-html :html :head :title :body :p)
  (:import-from :easy-routes :defroute)
  (:import-from :lisp-fixup :with-output-to-real-string))

(in-package :linnarope.views.root)

(defroute root ("/" :method :get :decorators (@html)) ()
  (html
   (head
    (title "Linnarope resource manager"))
   (body
    (let ((x 66))
      (restored-stdout
       (format t "x times three is: ~a~%" (* x 3)))
      (p "X is " (prin1-to-string x))))))
