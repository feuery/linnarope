(format t "Hello from tetris ~%")

(defpackage examples.tetris
  (:use :cl :engine))

(in-package :examples.tetris)

(defparameter *valid-blocks*
  (list :square :l :l-reverse :s :s-reverse :straight))

(defclass block ()
  ((x :initarg :x :initform (error "wish i had an x") :accessor block-x)
   (y :initarg :y :initform (error "wish i had an y") :accessor block-y)
   (form :initarg :form :initform (error "Wish i had a form") :accessor form)))

(defparameter *field-width-blocks* 10)
(defparameter *field-height-blocks* 20)

(defmethod initialize-instance :after ((b block) &key)
  (assert (member (form b) *valid-blocks*)))

(defmethod move-left ((b block))
  (decf (block-x b)))

(defmethod move-right ((b block))
  (incf (block-y b)))

(defun flatten-once (list)
  (mapcan (lambda (i) i) list))

(defun render-block (bl)
  (let ((block-sprite (get-resource "Lisp sprite" "block"))
	(bl-x (* 50 (block-x bl)))
	(bl-y (* 50 (block-y bl))))
    (unless block-sprite
      (format t "Rendering without block sprite :/~%"))
    (when block-sprite
      (labels ((draw (coords)
		 (dolist (coordinate (flatten-once coords))
		   (destructuring-bind (x . y) coordinate
		     (let ((dst-x (+ bl-x (* x 50)))
			   (dst-y (+ bl-y (* y 50))))
		       (render block-sprite dst-x dst-y))))))
	
	(case (form bl)
	  (:square
	   ;; 4 blokkia, neliömuodostelmassa
	   (draw (loop for x from 0 to 1
		       collecting
		       (loop
			 for y from 0 to 1
			 collecting (cons x y)))))

	  (:straight
	   (draw (loop for x from 0 to 0
		       collecting
		       (loop 
			 for y from 0 to 2			   
			 collecting (cons x y)))))

	  (:s-reverse
	   (draw (loop for x from 0 to 2
		       collecting
		       (loop 
			 for y from 0 to 1
			 when (or (eq x 1)
				  (and (eq x 2) (eq y 1))
				  (and (eq x 0) (eq y 0)))
			   collecting (cons x y)))))

	  (:s
	   (draw (loop for x from 0 to 2
		       collecting
		       (loop 
			 for y from 0 to 1
			 when (or (eq x 1)
				  (and (eq x 0 ) (eq y 1))
				  (and (eq x 2) (eq y 0)))
			   collecting (cons x y)))))
	  
	  (:l-reverse 
	   (draw
	    (loop for x from 0 to 1 
		  collecting
		  (loop 
		    for y from 0 to 2
		    when (or (eq x 1) (eq y 0))
		      collecting (cons x y)))))
	  (:l
	   (draw
	    (loop for x from 0 to 1 
		  collecting
		  (loop 
		    for y from 0 to 2
		    when (or (eq x 0) (eq y 2))
		      collecting (cons x y))))))))))

(defparameter *last-updated* (mstimer))

(defparameter *current-block* nil)

(defun update (current-map)
  (when *current-block*

    (when (> (- (mstimer) *last-updated*) 1000)
      (incf (block-y *current-block*))
      (setf *last-updated* (mstimer)))

    (when (keydown? "SDLK_LEFT")
      (decf (block-x *current-block*)))

    (when (keydown? "SDLK_RIGHT")
      (incf (block-x *current-block*)))
    
    (render current-map 0 0)

    (render-block *current-block*)

    (draw-line 130 50 50 200 1)))

(defun setup-tetris ()
  (format t "Set up tetris! ~%"))

(setf *current-block* (make-instance 'block :x 0 :y 0 :form :square))

(setup-scene
 ;; I think setup fns are completely unnecessary 
 #'setup-tetris
 #'update
 
 (lambda ()
   (format t "Closing tetris ~%")))
