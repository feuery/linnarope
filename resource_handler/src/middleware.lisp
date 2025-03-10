(defpackage linnarope.middleware
  (:use :cl)
  (:import-from :lisp-fixup :with-output-to-real-string)
  (:import-from :easy-routes :defroute)
  (:export :list-all-js-resources :js-resource :defsubtab :deftab :tabs :@html :@db :*connection* :*database-name*))

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
    "A list of tabs, keyed by symbol and valued by their urls"))

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
	 (populated-html (mustache:render* (html-resource "root.html") (cons
									`(:tabs . ,(tabs-for-view current-tab-url tabs))
									(cons
									 (cons :component (mustache:render* (html-resource component) component-data))
									 component-data)))))
    (setf (hunchentoot:content-type*) "text/html")
    populated-html))

(defmacro deftab (varlist &rest contents)
  (destructuring-bind (tab-symbol tab-url component-filename) varlist
    (setf (gethash tab-symbol tabs) tab-url)
    `(defroute ,tab-symbol (,tab-url :method :get :decorators ((@html ,tab-url) @db)) ()
       (cons (cons :component ,component-filename)
	     (progn
	       ,@contents)))))

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

(defvar *connection*
  nil)

(defvar *database-name* (format nil "~aresources.db" *system-source-directory*))

(defun @db (next)
  (dbi:with-connection (conn :sqlite3 :database-name *database-name*)
    (let ((*connection* conn))
      ;; why is this not committing?
      ;; (dbi:with-transaction *connection*
      (let ((result (funcall next)))
	;; (dbi:commit *connection*)
	result))))

;; (getf (first 
;;        (@db (lambda ()
;; 	      (cl-dbi:fetch-all 
;; 	       (cl-dbi:execute (cl-dbi:prepare *connection* "SELECT count(*) as c FROM map")
;; 			       (list ))))))
;;       :|c|) ;; => 0
