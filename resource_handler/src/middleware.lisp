(defpackage linnarope.middleware
  (:use :cl)
  (:import-from :lisp-fixup :with-output-to-real-string)
  (:import-from :easy-routes :defroute)
  (:export :defheaderbutton :list-all-js-resources :js-resource :defsubtab :deftab :tabs :@html :@db :*database-name*))

(in-package :linnarope.middleware)

(defun alist-get (alist key)
  (cdr (assoc key alist)))

(defvar *resource-path* (pathname (format nil "~aresources/html/" (asdf:system-source-directory "linnarope-resource-handler"))))
(defvar *js-resource-path* (pathname (format nil "~aresources/js/" (asdf:system-source-directory "linnarope-resource-handler"))))

(defun html-resource (filename)
  (lisp-fixup:slurp-utf-8 
   (pathname (format nil "~a/~a" *resource-path* filename))))

(defun js-resource (filename)
  (lisp-fixup:slurp-utf-8 
   (pathname (format nil "~a/~a" *js-resource-path* filename))))

(defvar *js-resource-path* (pathname (format nil "~aresources/js/" (asdf:system-source-directory "linnarope-resource-handler"))))

(defun list-all-js-resources ()
  (cl-fad:list-directory 
   *js-resource-path*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar tabs (make-hash-table :test 'equal)
    "A list of tabs, keyed by symbol and valued by their urls")
  (defvar buttons (make-hash-table :test 'equal)
    "A list of header buttons"))

(defun tabs-for-view (current-tab-url tabs)
  (mapcar (lambda (cell)
	    (destructuring-bind (sym . url) cell
	      `((:name . ,(string-capitalize (symbol-name sym)))
		(:selected . ,(string= current-tab-url url))
		(:url  . ,url))))
	  
	  (alexandria:hash-table-alist
	   tabs)))

(defun @html (current-tab-url next)
  "Expects next to return '((:a . alist) (:of . data)) that populates {{mustache-placeholders}} in /resources/html. :component is a key that specifies which template is chosen to be set up inside root.html"
  (let* ((component-data (funcall next))
	 (component (alist-get component-data :component))
	 (mustache:*escape-tokens* nil)
	 (js-files (cons `((:src . "root.js"))
			 (alist-get component-data :js-files)))
	 (params (concatenate 'list
			      `((:js-files . ,js-files)
				(:tabs . ,(tabs-for-view current-tab-url tabs))
				(:buttons . ,(map 'list
						  (lambda (cell)
						    (destructuring-bind (id . body) cell
						      `((:id . ,(str:downcase id))
							(:body . ,body))))
						  (alexandria:hash-table-alist buttons)))
				(:component . ,(mustache:render* (html-resource component) component-data)))
			      component-data))
	 (populated-html (mustache:render* (html-resource "root.html") params )))
    (setf (hunchentoot:content-type*) "text/html")
    populated-html))

(defmacro deftab (varlist &rest contents)
  (destructuring-bind (tab-symbol tab-url component-filename) varlist
    (setf (gethash tab-symbol tabs) tab-url)
    `(defroute ,tab-symbol (,tab-url :method :get :decorators ((@html ,tab-url) @db)) ()
       (cons (cons :component ,component-filename)
	     (progn
	       ,@contents)))))

(defmacro defheaderbutton (id-symbol body-text)
  "(defheaderbutton btn-id \"Do Something fun!\") => generates a <button id=\"btn-id\">Do Something fun!</button> in the header"
  (setf (gethash id-symbol buttons) body-text))
      

(defmacro defsubtab (varlist decorators http-varlist &rest contents)
  (destructuring-bind (route-symbol route-url component-filename parent-tab . _) varlist
    ;; shut up plz :D
    (declare (ignore _))
    (let* ((tab-url (gethash parent-tab tabs))
	   (last-var (car (last varlist)))
	   (method (if (not (equalp last-var parent-tab))
		       last-var
		       :get)))
      `(defroute ,route-symbol (,route-url :method ,method :decorators ((@html ,tab-url) @db ,@decorators)) ,http-varlist
	 (cons (cons :component ,component-filename)
	       (progn
		 ,@contents))))))


(defun @css (next)
  (setf (hunchentoot:content-type*) "text/css")
  (funcall next))

;; database
(defvar *system-source-directory*
  (asdf:system-source-directory "linnarope-resource-handler"))

(defvar db-user "linnarope")
(defvar db-password "linnarope")
(defvar db-db "linnarope")
(defvar db-port 5432)
(defvar db-host "localhost")

(defvar db-config
  (list :db db-db 
	:username db-user
	:password db-password
	:host db-host 
	:port db-port))

(defun connect-toplevel ()
  (destructuring-bind (&key db username password host port) db-config
    (postmodern:connect-toplevel db username password host :port port)))

;; (connect-toplevel)

(defun @db (next)
  (destructuring-bind (&key db username password host port) db-config
    (postmodern:with-connection (list db username password host :port port)
      ;; (postmodern:with-transaction ()
	(funcall next))))
