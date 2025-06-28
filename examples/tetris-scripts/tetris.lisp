(defpackage examples.tetris
  (:use :cl :engine))

(format t "Hello from tetris ~%")

(in-package :examples.tetris)

(defparameter *valid-blocks*
  (list :square :l :l-reverse :s :s-reverse :straight :single))

(defclass block ()
  ((x :initarg :x :initform (error "wish i had an x") :accessor block-x)
   (y :initarg :y :initform (error "wish i had an y") :accessor block-y)
   (form :initarg :form :initform (error "Wish i had a form") :accessor form)))

(defparameter *field-width-blocks* 10)
(defparameter *field-height-blocks* 20)
(defparameter *block-w* 50)

(defmethod initialize-instance :after ((b block) &key)
  (assert (member (form b) *valid-blocks*)))

(defmethod move-left ((b block))
  (decf (block-x b)))

(defmethod move-right ((b block))
  (incf (block-y b)))

(defmethod width ((b block))
  (case (form b)
    (:single 1)
    (:square 2)
    (:straight 1) ;; h = 3
    (:s-reverse 3)
    (:s 3)
    (:l 2)
    (:l-reverse 2)
    (t (error "unknown block"))))

(defmethod height  ((b block))
  (case (form b)
    (:square 2)
    (:straight 3)
    (:s-reverse 2)
    (:s 2)
    (:l 3)
    (:l-reverse 3)
    (:single 1)
    (t (error "unknown block"))))

;; these might get called during re-evaluating the block class
(defmethod height ((_ t))
  (declare (ignore _))
  0)

(defmethod width ((_ t))
  (declare (ignore _))
  0)

(defun flatten-once (list)
  (mapcan (lambda (i) i) list))

(defmethod all-block-coords ((bl block))
  (flatten-once
   (case (form bl)
     (:single
      (loop for x from 0 to 0
	    collecting
	    (loop for y from 0 to 0
		  collecting (cons x y))))
     (:square
      ;; 4 blokkia, neliömuodostelmassa
      (loop for x from 0 to 1
	    collecting
	    (loop
	      for y from 0 to 1
	      collecting (cons x y))))

     (:straight
      (loop for x from 0 to 0
	    collecting
	    (loop 
	      for y from 0 to 2			   
	      collecting (cons x y))))

     (:s-reverse
      (loop for x from 0 to 2
	    collecting
	    (loop 
	      for y from 0 to 1
	      when (or (eq x 1)
		       (and (eq x 2) (eq y 1))
		       (and (eq x 0) (eq y 0)))
		collecting (cons x y))))

     (:s
      (loop for x from 0 to 2
	    collecting
	    (loop 
	      for y from 0 to 1
	      when (or (eq x 1)
		       (and (eq x 0 ) (eq y 1))
		       (and (eq x 2) (eq y 0)))
		collecting (cons x y))))
     
     (:l-reverse 
      (loop for x from 0 to 1 
	    collecting
	    (loop 
	      for y from 0 to 2
	      when (or (eq x 1) (eq y 0))
		collecting (cons x y))))
     (:l
      (loop for x from 0 to 1 
	    collecting
	    (loop 
	      for y from 0 to 2
	      when (or (eq x 0) (eq y 2))
		collecting (cons x y)))))))

(defun render-block (bl)
  (when bl
    (let ((block-sprite (engine:get-resource "Lisp sprite" "block"))
	  (bl-x (* 50 (block-x bl)))
	  (bl-y (* 50 (block-y bl))))
      (unless block-sprite
	(format t "Rendering without block sprite :/~%"))
      (when block-sprite
	(labels ((draw (coords)
		   (dolist (coordinate coords)
		     (destructuring-bind (x . y) coordinate
		       (let ((dst-x (+ bl-x (* x 50)))
			     (dst-y (+ bl-y (* y 50))))
			 (engine:render block-sprite dst-x dst-y))))))
	  (draw (all-block-coords bl)))))))

(defparameter *last-updated* (engine:mstimer))

(defparameter *current-block* nil)

(defparameter map-x 0)
(defparameter map-y 0)

(defparameter finished? nil)

(defparameter background-blocks (make-hash-table :test 'equal))

(defun merge-and-respawn-block ()
  (unless *current-block*
    (format t "Wtf how is *current-block nil?~%"))
  (let* ((x (block-x *current-block*))
	 (y (block-y *current-block*))
	 (all-block-coordinates (all-block-coords *current-block*)))
    (dolist (cc all-block-coordinates)
      (destructuring-bind (x1 . y1) cc
	(let ((x (+ x x1))
	      (y (+ y y1)))
	  (setf (gethash (cons x y) background-blocks) (make-instance 'block :x x :y y :form :single)))))

    
    (setf *current-block* (make-instance 'block :x 0 :y 0 :form :square ;; (nth 
								 ;; (random (length *valid-blocks*))
								 ;; *valid-blocks*)
					 ))))

;; (incf (block-y *current-block*))


(defun block-collides-with-earth ()
  (when *current-block*
    (> (1+ (+ (block-y *current-block*) (height *current-block*)))
       *field-height-blocks*)))

(defun block-collides-with-other-block ()
  (when *current-block*
    (let* ((x (block-x *current-block*))
	   (y (1+ (block-y *current-block*)))
	   (contained-blocks (map 'list (lambda (p)
					  (destructuring-bind (xx . yy) p
					    (cons (+ x xx)
						  (+ y yy))))
				  (all-block-coords *current-block*))))
      (dolist (c contained-blocks)
	(maphash (lambda (bg-coord blocks)
		   (declare (ignore blocks))
		   (when (equalp c bg-coord)
		     (return-from block-collides-with-other-block t)))
		 background-blocks))
      nil)))

(defun block-on-left-edge? ()
  (zerop (block-x *current-block*)))

(defun block-on-right-edge? ()
  (> (+ (block-x *current-block*)
	(width *current-block*))
     (1- *field-width-blocks*)))

(defun draw-background ()
  (maphash (lambda (coordinates block)
	     (declare (ignore coordinates))
	     (when block
	       (render-block block)))
	   background-blocks)

  (engine:set-color 255 0 0)
  
  (engine:draw-line 0 0 (* *field-width-blocks* *block-w*) 0 10)
  (engine:draw-line 0 (* *field-height-blocks* *block-w*)
	     (* *field-width-blocks* *block-w*) (* *field-height-blocks* *block-w*)
	     10)
  (engine:draw-line (* *field-width-blocks* *block-w*) 0
	     (* *field-width-blocks* *block-w*) (* *field-height-blocks* *block-w*) 10)
  (engine:draw-line 0 0
	     0 (* *field-height-blocks* *block-w*) 10)

  (engine:set-color 0 0 0))

(defun do-stupid-stuff-with-map (current-map)
  
  (engine:render current-map (decf map-x) (decf map-y))

  (when (< map-x -900)
    (setf map-x 900))

  (when (< map-y -900)
    (setf map-y 900)))

(defun handle-kbd ()
  (if (and (not (block-on-left-edge?))
	   (engine:keydown? "SDLK_LEFT"))
      (decf (block-x *current-block*))
      )

  (when (and (not (block-on-right-edge?))
	     (engine:keydown? "SDLK_RIGHT"))
    
    (incf (block-x *current-block*))))

(defun handle-full-rows ()
  "Checks if there are completely filled horizontal rows that need to disappear. If there are, this function makes them disappear"

  (let ((acc))
    (dotimes (y *field-height-blocks*)
      (let ((counter 0))
	(dotimes (x *field-width-blocks*)
	  (when (gethash (cons x y) background-blocks)
	    (incf counter)))
	(when (equalp counter *field-width-blocks*)
	  (push y acc))))
    ;; (when acc
    ;;   (format t "Rows ~a seem to be full~%" acc))

    (dolist (y acc)
      (dotimes (x *field-width-blocks*)
	(format t "Removing block at ~a, ~a~%" x y)
	(remhash (cons x y) background-blocks))
      
      ;; (dotimes (yy y)
      ;; 	(dotimes (x *field-width-blocks*)
      ;; 	  (let ((block* (gethash (cons x yy) background-blocks)))
      ;; 	    (remhash (cons x yy) background-blocks)
      ;; 	    (setf (gethash (cons x (1+ yy)
      
      (dotimes (yy y)
	;; we're moving blocks down bottoms-up
	(let ((actual-y-index (- y yy)))
	  (format t "We're moving blocks at y ~a~%" actual-y-index)
	  (dotimes (x *field-width-blocks*)
	    (let ((block* (gethash (cons x actual-y-index) background-blocks)))
	      (if block*
		  (progn
		    (format t "Moving stuff up from ~a => ~a~%" actual-y-index (1+ actual-y-index))
		    ;; why does this fuck everything up?
		    (remhash (cons x actual-y-index) background-blocks)
		    (setf (gethash (cons x (1+ actual-y-index)) background-blocks) block*))

		  ;; this is expected 
		  ;; (format t "Wtf no block at ~a~%" (cons x actual-y-index))
		  ))))))))

(defun update-game (current-map)

  (handle-full-rows)
  
  (when (or (block-collides-with-earth)
	    (block-collides-with-other-block))
    ;; (setf finished? t)

    (merge-and-respawn-block)
    ;; (return-from update-game)
    )

  (when (> (- (engine:mstimer) *last-updated*) 1000)
    (incf (block-y *current-block*))
    (setf *last-updated* (engine:mstimer)))

  (handle-kbd)

  ;; (format t "block-x ~d~%" (block-x *current-block*))

  (do-stupid-stuff-with-map current-map)

  (render-block *current-block*)

  ;; draw the background
  (draw-background))

(defun update-finished ()
  (engine:set-color 255 0 0)
  (engine:draw-text "FINISHED!" 100 100)
  (engine:set-color 0 0 0))
    
(defun update (current-map)
  (when *current-block*
    (if finished?
	(update-finished)
	(update-game current-map))))

(defun setup-tetris ()
  (format t "Set up tetris! ~%"))

(setf *current-block* (make-instance 'block :x 0 :y 0 :form :square))

(engine:setup-scene
 ;; I think setup fns are completely unnecessary 
 #'setup-tetris
 #'update
 
 (lambda ()
   (format t "Closing tetris ~%")))
