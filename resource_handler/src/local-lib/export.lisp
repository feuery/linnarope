(defpackage net.feuerx.export
  (:use :cl)
  (:documentation
   "A library that lets user compose a set of files (that exist in the fs, so a set of pathnames in the api) into a single file.

This library exists due to cl-dbi crashing when I tried to insert blobs into sqlite and me not being smart enough to use any of the zip- and tar-libraries that already exist.

The format of the final file is NAMELEN,FILENAME,FILELEN,CONTENTS without any delimiters.

Every function deals with the implicit *current-export-file* parameter"))

(in-package :net.feuerx.export)

;; (defvar *current-export-file* nil)

;; (defmacro with-new-file (export-filename &rest body)
;;   (let ((f (gensym)))
;;     `(with-open-file (,f ,export-filename :if-exists :supersede
;; 					  :direction :output
;; 					  :element-type '(unsigned-byte 32)
;; 					  :if-does-not-exist :create)
;;        (let ((*current-export-file* ,f))
;; 	 ,@body))))

;; (defmacro with-existing-file (export-filename &rest body)
;;   (let ((f (gensym)))
;;     `(with-open-file (,f ,export-filename :direction :input
;; 					  :element-type '(unsigned-byte 32)
;; 					  :if-does-not-exist :error)
;;        (let ((*current-export-file* ,f))
;; 	 ,@body))))

;; (defun assert-sane? ()
;;   (assert *current-export-file* nil "Are you inside either with-new-file or with-existing-file?"))

;; (defun append-file (filename)
;;   (assert-sane?)
;;   (let* ((filename-string (format nil "~a" (lisp-fixup:filename filename)))
;; 	 (name-len (length filename-string )))
;;     ;; (with-open-file (f filename :direction :input
;;     ;; 				:if-does-not-exist :error)
;;       (let* ((contents (lisp-fixup:slurp-bytes filename))
;; 	     (data-len (array-total-size contents)))
;; 	(write-sequence (make-array 1 :initial-element name-len) *current-export-file*)
;; 	(write-sequence (sb-ext:string-to-octets filename-string) *current-export-file*)
;; 	(write-sequence (make-array 1 :initial-element data-len)  *current-export-file*)
;; 	(write-sequence contents *current-export-file*))))

;; (defun entries ()
;;   (assert-sane?)
;;   (let ((name-len (make-array 1))
;; 	(data-len (make-array 1))
;; 	(offset 0))
;;     (read-sequence name-len *current-export-file* :start offset :end (+ offset 1))
;;     (incf offset)
    
;;     (let* ((actual-name-len (aref name-len 0))
;; 	   (name (make-array actual-name-len))
;; 	   (name-vec nil))
;;       (read-sequence name *current-export-file* :start offset :end (+ offset actual-name-len))
;;       (incf offset actual-name-len)

;;       (setf name-vec (coerce name '(vector (unsigned-byte 8))))
      
;;       (format t "entry name: ~a~%" (sb-ext:octets-to-string name-vec)))))
    
  
;; (with-existing-file "/Users/feuer/Projects/finrope/ilpotestaa"
;;   (entries))


;; (with-new-file "/Users/feuer/Projects/finrope/ilpotestaa"
;;   (dolist (fname 
;; 	   (list #P"/Users/feuer/Projects/finrope/resource_handler/resources/linnarope.tmp/warp-connections/1.json"
;; 		 #P"/Users/feuer/Projects/finrope/resource_handler/resources/linnarope.tmp/sprites/taysi_maailma.png"
;; 		 #P"/Users/feuer/Projects/finrope/resource_handler/resources/linnarope.tmp/palettes/aaaaaa.json"
;; 		 #P"/Users/feuer/Projects/finrope/resource_handler/resources/linnarope.tmp/maps/pikkustadi-toolon tulli.tmx"
;; 		 #P"/Users/feuer/Projects/finrope/resource_handler/resources/linnarope.tmp/maps/pikkustadi-toolon tulli.2.tmx"
;; 		 #P"/Users/feuer/Projects/finrope/resource_handler/resources/linnarope.tmp/lisp-sprites/aaaa.json"))
;;     (append-file fname)))
