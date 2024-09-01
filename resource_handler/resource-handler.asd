(require 'asdf)
(in-package :asdf-user)

(defsystem "linnarope-resource-handler"
  :author "Ilpo Lehtinen"
  :licence "GPLv3"
  :depends-on ("binding-arrows"
	       "hunchentoot"
	       "ironclad"
	       "trivial-utf-8"
	       "cl-ppcre"
	       "com.inuoe.jzon"
	       "easy-routes"
	       "str"
	       "cl-fad"
	       "log4cl"
	       "cl-dbi"
	       "alexandria"
	       "cl-css"
	       "cl-mustache")
  :description "A resource handler for linnarope game"
  :components ((:module "src"
		:components
		((:module "local-lib"
		  :components ((:file "lisp-fixup")))
		 (:file "middleware")
		 (:file "migrations")
		 (:module "db"
		  :components ((:file "maps")))
		 (:module "views"
		  :components ((:file "root")))
		 (:file "main")))))

;; (asdf:make "linnarope-resource-handler") 
