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
	       "xmls"
	       "cl-css"
	       "cl-mustache"
	       "parse-number"
	       "quri"
	       "cl-hash-util"
	       "postmodern")
  :description "A resource handler for linnarope game"
  :components ((:module "src"
		:components
		((:module "local-lib"
		  :components ((:file "lisp-fixup")))
		 (:file "middleware")
		 (:file "migrations")
		 (:file "tmxreader")
		 (:file "tsxreader")
		 (:module "db"
		  :components ((:file "maps")
			       (:file "scripts")
			       (:file "sprites")
			       (:file "palettes")))
		 (:module "views"
		  :components ((:file "root")
			       (:file "sprite")
			       (:file "export")
			       (:file "complete-map")
			       (:file "script")))
		 (:module "routes"
		  :components ((:file "sprite")))
		 (:file "main")))))

;; (ql:quickload "linnarope-resource-handler")
;; (linnarope.main:start-server)

;; or alternatively do this 
;; (asdf:make "linnarope-resource-handler")
