(defpackage linnarope.views.root
  (:use :cl)
  (:import-from :linnarope.middleware :@db :*connection* :@html :@css :restored-stdout)
  (:import-from :macro-html :html :head :link :meta :title :body :p :div)
  (:import-from :easy-routes :defroute)
  (:import-from :lisp-fixup :with-output-to-real-string))

(in-package :linnarope.views.root)

(named-readtables:in-readtable macro-html:syntax)

(defroute css ("/css" :method :get :decorators (@css)) ()
  (cl-css:css '((body
		 :background-color "#000"
		 :color "#fff"))))

(defroute root ("/" :method :get :decorators (@html @db)) ()
  (let ((count (getf 
		(first
		 (cl-dbi:fetch-all
		  (cl-dbi:execute (cl-dbi:prepare *connection* "SELECT count(*) AS \"count-maps\" FROM map"))))
		:|count-maps|)))
    (html
     (head
      (title "Linnarope resource manager")
      (meta :charset "utf-8")
      (link :href "/css"  :rel "stylesheet" :type "text/css"))
     (body
      (div [ :class "maps"]
	   (prin1-to-string count) " maps in the system. Add more?")))))
