;; finrope/resource_handler/src/local-lib
(defpackage lisp-fixup
  (:use :cl)
  (:export :partial
	   :hashtable-merge
	   :with-output-to-real-string
	   :compose :drop
	   :slurp-bytes :slurp-utf-8))

(in-package :lisp-fixup)

;; https://www.n16f.net/blog/reading-files-faster-in-common-lisp/
(defun slurp-bytes (path)
  (declare (type (or pathname string) path))
  (let ((data (make-array 0 :element-type '(unsigned-byte 8) :adjustable t))
        (block-size 4096)
        (offset 0))
    (with-open-file (file path :element-type '(unsigned-byte 8))
      (loop
        (let* ((capacity (array-total-size data))
               (nb-left (- capacity offset)))
          (when (< nb-left block-size)
            (let ((new-length (max (+ capacity (- block-size nb-left))
                                   (floor (* capacity 3) 2))))
              (setf data (adjust-array data new-length)))))
        (let ((end (read-sequence data file :start offset)))
          (when (= end offset)
            (return-from slurp-bytes (adjust-array data end)))
          (setf offset end))))))

(defun slurp-utf-8 (path)
  (trivial-utf-8:utf-8-bytes-to-string (slurp-bytes path)))

(defun drop (n lst)
  "Returns a sequence that skips the first N elements of the given list."
  (cond ((or (null lst) (<= n 0)) lst)
        ((> n 0) (drop (1- n) (cdr lst)))))

(defun partial (f &rest args)
  (lambda (&rest rst-args)
    (apply f (concatenate 'list args rst-args))))

(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (reduce #'funcall functions
              :initial-value x
              :from-end t)))

(defun hashtable-merge (m1 m2)
  "Destructively assigns everything in m2 into m1 and returns it"
  (dolist (cell (alexandria:hash-table-alist m2))
    (destructuring-bind (k . v) cell
      (setf (gethash k m1) v)))
  m1)
