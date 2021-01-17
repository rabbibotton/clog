(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-demo))

(in-package :clog-user)

;; Game Display
(defconstant display-width 400)
(defconstant display-height 400)

;; Snake Parameters
(defconstant initial-length 5)
(defconstant segment-size 10)

(deftype snake-direction-type () '(member :left :right :up :down))

(defun new-food ()
  (list (random (floor (- (/ display-width segment-size) 1))) 
	(random (floor (- (/ display-height segment-size) 1)))))

(defclass app-data ()
  ((snake-direction
    :initform :right
    :type snake-direction-type
    :accessor snake-direction)
   (score
    :initform 0
    :type number
    :accessor score)
   (food
    :initform (new-food)
    :type cons
    :accessor food)
   (snake
    :initform nil
    :type cons
    :accessor snake)))

(defun display-splash (body)
  (let* ((splash
	   (create-div body :content
"<H1>(Sparky The Snake)</H1>
 <br />
 <p>Use your kebyoard to move Sparky to pick up batteries.</p>
 <i>Be careful...</i><br />
 If sparky hits his tail he electrocute himself to <b>death!!</b>
 <br /><br />
  Use the arrow keys or a,w,s,d for direction keys.<br/><br/>"))
	 (ticker (create-span splash)))
    
    (setf (width splash) "100%")
    (setf (text-alignment splash) :center)
    (dotimes (n 10)
      (setf (text ticker) (format nil "~A *" (text ticker)))
      (sleep .1))
    (setf (hiddenp splash) t)))

(defun paint (cx app)
  (let ((game-over nil)
	(head-cell (car (snake app))))

    (flet ((draw-segment (cell)
	     (fill-rect cx
			(* (car cell) segment-size)
			(* (cadr cell) segment-size)
			segment-size
			segment-size))
	   
	   (self-collision ()
	     (dolist (cell (snake app))
	       (when (equal cell head-cell)
		 (return t)))))

      (cond ((eq :right (snake-direction app))
	      (setf head-cell (list (1+ (car head-cell))
				    (cadr head-cell))))
	    ((eq :left (snake-direction app))
	      (setf head-cell (list (1- (car head-cell))
				    (cadr head-cell))))
	    ((eq :up (snake-direction app))
	      (setf head-cell (list (car head-cell)
				    (1- (cadr head-cell)))))
	    ((eq :down (snake-direction app))
	      (setf head-cell (list (car head-cell)
				    (1+ (cadr head-cell))))))

      
      (cond ((or (< (car head-cell) 0)
	      (< (cadr head-cell) 0)
	      (>= (* (car head-cell) segment-size) display-width)
	      (>= (* (cadr head-cell) segment-size) display-height)
	      (self-collision))

	     (fill-style cx :red)
	     (font-style cx "bold 20px sans-serif")
	     (fill-text cx "GAME OVER" 30 30)
	     (setf game-over t))
	    (t
	     (fill-style cx :purple)
	     (push head-cell (snake app))
	     (dolist (cell (snake app))
	       (draw-segment cell))

	     (fill-style cx :white)

	     (cond ((equal head-cell (food app))

		    (fill-text cx (format nil "Score: ~A" (score app))
			       5 (- display-height 15))
		    (setf (score app) (+ (score app) 10))
		    
		    (fill-style cx :green)
		    (fill-text cx (format nil "Score: ~A" (score app))
			       5 (- display-height 15))
		    
		    (setf (food app) (new-food)))
		   (t
		    (draw-segment (car (last (snake app))))
		    (setf (snake app) (butlast (snake app)))))
	  
	     (fill-style cx :brown)
	     (draw-segment (food app)))
      
	game-over))))

(defun on-key-down (obj event)
  (let ((app      (connection-data-item obj "app-data"))
	(key-code (getf event :key-code)))

    (cond ((or (eql key-code 65) (eql key-code 37)) (setf (snake-direction app) :left))
	  ((or (eql key-code 87) (eql key-code 38)) (setf (snake-direction app) :up))
	  ((or (eql key-code 83) (eql key-code 40)) (setf (snake-direction app) :down))
	  ((or (eql key-code 68) (eql key-code 39)) (setf (snake-direction app) :right)))))
	  

(defun start-game (body)
  (let ((app (connection-data-item body "app-data"))
	(disp (create-canvas body
			     :width display-width
			     :height display-height))
	context)

    ;; Initialize display
    (setf (background-color body) :orange)
    
    (setf (display disp) :block)
    (setf (background-color disp) :white)
    (set-margin disp :auto :auto :auto :auto)
    (set-border disp :thin :solid :white)
    (setf (border-radius disp) "10px")
    (setf (box-shadow disp) "3px 3px 5px")

    ;; Initialize snake
    (dotimes (n initial-length)
      (push (list n 0) (snake app)))

    (setf context (create-context2d disp))
    (font-style context "normal 20px sans-serif")
    (fill-style context :green)
    (fill-text context (format nil "Score: ~A" (score app))
	       5 (- display-height 15))

    (set-on-key-down body #'on-key-down)
    
    ;; Game loop
    (loop
      (unless (validp body) (return))
      (when (paint context app) (return))
      (sleep .1))))

(defun on-new-window (body)
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app))
  
  (display-splash body)
  (start-game body))

(defun start-demo ()
  "Start demo."

  (initialize #'on-new-window)
  (open-browser))
