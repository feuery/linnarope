(defpackage linnarope.views.complete-map
  (:use :cl)
  (:import-from :lisp-fixup :filename)
  (:import-from :easy-routes :defroute)
  (:import-from :linnarope.db.maps :*whole-map-png-location* :generate-whole-map-png)
  (:import-from :linnarope.middleware :@db :deftab :defsubtab))

(in-package :linnarope.views.complete-map)


(deftab (EntireMap "/complete-map" "complete-map.html")
    (generate-whole-map-png)
  nil)

(defroute img-whole-map ("/img/complete-map" :method :get) ()
  (let ((bytes (lisp-fixup:slurp-bytes *whole-map-png-location*)))
    (setf (hunchentoot:content-type*) "image/png")
    bytes))
