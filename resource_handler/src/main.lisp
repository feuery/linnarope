(defpackage linnarope.main
  (:use :cl)
  (:export :start-server :main))

(in-package :linnarope.main)

(defvar *server* nil)
(setf hunchentoot:*catch-errors-p* nil)

(defun start-server (&key (port 3010))
  (format t "Starting linnarope resource server~%")
  (let ((server (make-instance 'easy-routes:easy-routes-acceptor :port port)))
    (format t "Server exists~%")
    (when (equalp 3010 port)
      (setf *server* server))
    
    (hunchentoot:start server)
    (format t "Hunchentoot:started server~%")
    
    (linnarope.migrations:migrate)
    (format t "migrated~%")
    
    (format t "Started linnarope resource server on ~a ~%" port)
    server))

(defun main (&key (port 3000))
  (start-server :port port)
  (handler-case
      (loop do (sleep 1000))
    (condition () nil)))

(format t "Linnerope loaded ~%")
  
