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
	       "alexandria"
	       "macro-html")
  :description "A resource handler for linnarope game"
  :components ((:module "src"
		:components
		((:module "local-lib"
		  :components ((:file "lisp-fixup")))
		 (:file "middleware")
		 (:module "views"
		  :components ((:file "root")))
		 (:file "main")))))
