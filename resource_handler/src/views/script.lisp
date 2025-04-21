(defpackage linnarope.views.script
  (:use :cl)
  (:import-from :cl-hash-util :with-keys)
  (:local-nicknames (:json :com.inuoe.jzon))
  (:import-from :easy-routes :defroute)
  (:import-from :linnarope.middleware :@db :deftab :defsubtab))

(in-package :linnarope.views.script)

(deftab (scripts "/scripts" "script-list.html")
    `((:scripts .,(postmodern:query "SELECT ID, name from script" :alists))))

(defun add-file-extension (script-name)
  "Makes sure `script-name` ends with .lisp and only one .lisp (adds the extension if it's missing and removes duplicates if replicated for some reason"
  (cl-ppcre:regex-replace "(.lisp)*$" script-name ".lisp"))

(defroute new-script ("/new-script" :method :post :decorators (@db)) (&post new-name)
  (postmodern:execute "INSERT INTO script (name, script) VALUES ($1, '')" (add-file-extension new-name))
  (easy-routes:redirect 'scripts))
  
(defsubtab (edit-script "/script/:id" "edit-script.html" scripts) () ()
  (with-keys ("name" "id" "script") (first (coerce (postmodern:query "SELECT * FROM script WHERE id = $1" id :array-hash) 'list))
    `((:name . ,name)
      (:id . ,id)
      (:script . ,(str:replace-all
		   "\\n" (format nil "~%")
		   (str:replace-all "\"" "&quot;" script)))
      (:js-files . (((:src . "https://unpkg.com/ace-custom-element@latest/dist/index.min.js")
		     (:type . "module")))))))

(defroute update-script ("/script/:id" :method :post :decorators (@db)) ()
  (let ((url-id id))
    (with-keys ("id" "name" "content") (json:parse (hunchentoot:raw-post-data :force-text t))
      (assert (equalp (prin1-to-string id) url-id))
      (postmodern:execute "UPDATE script SET name = $1, script = $2 WHERE id = $3" (add-file-extension name) content id)    
      (setf (hunchentoot:return-code*) 204)
      "")))
