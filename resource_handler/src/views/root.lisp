(defpackage linnarope.views.root
  (:use :cl)
  (:import-from :linnarope.middleware :@db :*connection* :@html :@css :restored-stdout)
  (:import-from :macro-html :ul :li :html :head :link :meta :title :body :p :div :a)
  (:import-from :easy-routes :defroute)
  (:import-from :lisp-fixup :with-output-to-real-string))

(in-package :linnarope.views.root)

(named-readtables:in-readtable macro-html:syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar tabs (make-hash-table :test 'equal)
    "A list of tabs, keyed by symbol and valued by their urls"))

(defroute css ("/css" :method :get :decorators (@css)) ()
  (cl-css:css `((body
		 :background-color "#fff")
		(.topbar
		 :display "flex"
		 :justify-content "space-evenly"
		 :list-style "none"
		 :flex-direction "row")
		(".topbar li"
		 :border-bottom "2px solid black")
		(".topbar li.selected"
		 :border-bottom "5px solid black"))))

(defmacro root-component (current-tab-symbol &rest contents)
  `(html
    (head
     (title "Linnarope resource manager")
     (meta :charset "utf-8")
     (link :href "/css"  :rel "stylesheet" :type "text/css"))
    (body
     (ul [:class "topbar" ]
	 (loop for k being the hash-keys of tabs
	       for url = (gethash k tabs )
	       collect (li [:class (if (equal ,current-tab-symbol k) "selected" "")]
			   (a [:href url] (string-upcase k)))))
     ,@contents)))

(defmacro deftab (varlist &rest contents)
  (destructuring-bind (tab-symbol tab-url) varlist
    (setf (gethash tab-symbol tabs) tab-url)
    `(defroute ,tab-symbol (,tab-url :method :get :decorators (@html @db)) ()
       (root-component (quote ,tab-symbol) ,@contents))))

(deftab (root "/")
    (let ((count (getf 
		  (first
		   (cl-dbi:fetch-all
		    (cl-dbi:execute (cl-dbi:prepare *connection* "SELECT count(*) AS \"count-maps\" FROM map"))))
		  :|count-maps|)))
      (div
       (div "Hello from deftab")
       (div [ :class "maps"]
	    (prin1-to-string count)
	    " maps in the system. Add more?"))))

(deftab (sprites "/sprites")
    (div "Probably no sprites yet, as the table doesn't exist"))
