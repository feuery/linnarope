(defpackage linnarope.views.root
  (:use :cl)
  (:import-from :linnarope.middleware :@db :*connection* :@html :@css :deftab)
  (:import-from :easy-routes :defroute)
  (:import-from :lisp-fixup :with-output-to-real-string))

(in-package :linnarope.views.root)

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

(deftab (maps "/maps" "maps.html")
    '((:map-count . 765)))

(deftab (sprites "/sprites" "sprites.html")
    '((:sprite-data . "lolz")))

(defroute root ("/" :method :get) ()
  (easy-routes:redirect 'maps))
