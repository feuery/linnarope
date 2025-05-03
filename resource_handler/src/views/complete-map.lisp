(defpackage linnarope.views.complete-map
  (:use :cl)
  (:import-from :lisp-fixup :filename)
  (:import-from :easy-routes :defroute)
  (:import-from :linnarope.db.maps :*whole-map-png-location* :generate-whole-map-png)
  (:import-from :linnarope.middleware :@db :deftab :defsubtab))

(in-package :linnarope.views.complete-map)


(deftab (EntireMap "/complete-map" "complete-map.html")
    (let ((path (format nil "~aentiremap-export"
			(asdf:system-source-directory "linnarope-resource-handler"))))
      (delete-file path)
      (linnarope.views.export:export-project path)
      (assert (cl-fad:file-exists-p path))
      (generate-whole-map-png path)
      nil))

(defroute img-whole-map ("/img/complete-map" :method :get) ()
  (let ((bytes (lisp-fixup:slurp-bytes *whole-map-png-location*)))
    (setf (hunchentoot:content-type*) "image/png")
    bytes))
