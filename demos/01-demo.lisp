(defpackage #:clog-demo-1
  (:use #:cl #:clog)
  (:export start-demo))

(in-package :clog-demo-1)

;; Game Display
(defconstant display-width 375)
(defconstant display-height 375)

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
    :accessor snake-direction)
   (score
    :initform 0
    :accessor score)
   (food
    :initform (new-food)
    :accessor food)
   (snake
    :initform nil
    :accessor snake)))

(defun display-splash (body)
  (let* ((splash
	   (create-div body :content
"<H1>(Sparky The Snake)</H1>
 <br />
 <p>Use your keyboard to move Sparky to pick up batteries.</p>
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

(defun paint (body cx app)
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
	     (play-media (create-audio body :source "/demo/game-over.wav" :controls nil))
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
		    (play-media (create-audio body :source "/demo/eat.wav" :controls nil))
		    (setf (food app) (new-food)))
		   (t
		    (draw-segment (car (last (snake app))))
		    (setf (snake app) (butlast (snake app)))))	  
	     (fill-style cx :brown)
	     (draw-segment (food app))))      
	game-over)))

(defun on-key-down (obj event)
  (let ((app      (connection-data-item obj "app-data"))
	(key      (getf event :key)))
    (cond ((or (equalp key "ArrowLeft") (equalp key "a"))
	   (setf (snake-direction app) :left))
	  ((or (equalp key "ArrowUp") (equalp key "w"))
	   (setf (snake-direction app) :up))
	  ((or (equalp key "ArrowDown") (equalp key "s"))
	   (setf (snake-direction app) :down))
	  ((or (equalp key "ArrowRight") (equalp key "d"))
	   (setf (snake-direction app) :right)))))
	  
(defun on-click (obj)
  (let ((app     (connection-data-item obj "app-data"))
	(btn-txt (text obj)))
  (cond ((equal btn-txt "<--") (setf (snake-direction app) :left))
	((equal btn-txt "-->") (setf (snake-direction app) :right))
	((equal btn-txt "-^-") (setf (snake-direction app) :up))
	((equal btn-txt "-v-") (setf (snake-direction app) :down)))))

(defun start-game (body)
  (let* ((app       (connection-data-item body "app-data"))
	 (disp      (create-canvas body
				   :width display-width
				   :height display-height))
	 (br        (create-br body))
	 (controls  (create-div body))
	 (left-btn  (create-button controls :content "<--"))
	 (right-btn (create-button controls :content "-->"))
	 (up-btn    (create-button controls :content "-^-"))
	 (down-btn  (create-button controls :content "-v-"))
	 context)
    (declare (ignore br))
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
    (set-on-key-down body      #'on-key-down :disable-default t)
    (set-on-click    left-btn  #'on-click)
    (set-on-click    right-btn #'on-click)
    (set-on-click    up-btn    #'on-click)
    (set-on-click    down-btn  #'on-click)
    (play-media (create-audio body :source "/demo/start.wav" :controls nil))
    ;; Game loop
    (loop
      (unless (validp body) (return))
      (when (paint body context app) (return))
      (sleep .1))))

(defun on-new-window (body)
  (set-html-on-close body "Connection Lost")
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app))
  (display-splash body)
  (start-game body))

(defun start-demo ()
  "Start demo."
  (initialize #'on-new-window)
  (open-browser))
