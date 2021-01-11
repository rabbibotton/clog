(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defclass app-data ()
  ((drag-mutex
    :reader drag-mutex
    :initform (bordeaux-threads:make-lock)
    :documentation "Serialize access to the on-mouse-down event.")
   (in-drag
    :accessor in-drag-p
    :initform nil
    :documentation "Insure only one box is dragged at a time.")
   (drag-x
    :accessor drag-x
    :documentation "The location of the left side of the box relative to mouse during drag.")
   (drag-y
    :accessor drag-y
   :documentation "The location of the top of the box relative to mouse during drag."))
  (:documentation "App data specific to each instance of our tutorial 8 app"))

(defun on-mouse-down (obj data)
  (let ((app (connection-data-item obj "app-data")))    ; Access our instance of App-Data
    (bordeaux-threads:with-lock-held ((drag-mutex app)) ; Insurce the first event received
      (unless (in-drag-p app)                           ; to drag is the only one, ie only
	(setf (in-drag-p app) t)                        ; the innermost box is dragged.

      (let* ((mouse-x  (getf data ':screen-x))          ; Use the screen coordinents not
	     (mouse-y  (getf data ':screen-y))          ; the coordents relative to the obj
	     (obj-top  (parse-integer (top obj) :junk-allowed t))
	     (obj-left (parse-integer (left obj) :junk-allowed t)))
	
	(setf (drag-x app) (- mouse-x obj-left))
	(setf (drag-y app) (- mouse-y obj-top))
	
	(set-on-mouse-move obj 'on-mouse-move)
	(set-on-mouse-up obj 'stop-obj-grab)
	(set-on-mouse-leave obj 'on-mouse-leave)))))

(defun on-mouse-move (obj data)
  (let* ((app    (connection-data-item obj "app-data"))
	 (x      (getf data ':screen-x))
	 (y      (getf data ':screen-y))
	 (new-x  (- x (drag-x app)))
	 (new-y  (- y (drag-y app))))
    
    (setf (top obj) (format nil "~Apx" new-y))
    (setf (left obj) (format nil "~Apx" new-x)))))

(defun on-mouse-leave (obj)
  (let ((app (connection-data-item obj "app-data")))
    (setf (in-drag-p app) nil)
    (set-on-mouse-move obj nil)
    (set-on-mouse-up obj nil)
    (set-on-mouse-leave obj nil)))

(defun stop-obj-grab (obj data)
  (on-mouse-move obj data)
  (on-mouse-leave obj))

(defun on-new-window (body)
  (let ((app (make-instance 'app-data)))                 ; Create our "App-Data" for this instance
    (setf (connection-data-item body "app-data") app))   ; of our App.
  
  (setf (title (html-document body)) "Tutorial 8")

  (let* ((div1 (create-div body))
	 (div2 (create-div div1))
	 (div3 (create-div div2)))

    (set-border div1 :medium :solid :blue)
    (set-border div2 :thin :dotted :red)
    (set-border div3 :thick :dashed :green)

    (setf (width div1) 400)
    (setf (width div2) 300)
    (setf (width div3) 200)
    
    (setf (height div1) 400)
    (setf (height div2) 300)
    (setf (height div3) 200)

    (setf (positioning div1) :fixed)        ; It's location relative to window
    (setf (overflow div1) :hidden)          ; Clip the contents
    (set-on-mouse-down div1 'on-mouse-down)

    (setf (positioning div2) :absolute)     ; It's location relative to is parent container
    (setf (overflow div2) :hidden)
    (set-on-mouse-down div2 'on-mouse-down)

    (setf (positioning div3) :absolute)
    (set-on-mouse-down div3 'on-mouse-down)))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
