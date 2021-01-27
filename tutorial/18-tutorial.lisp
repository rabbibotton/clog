(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (let* ((target1 (create-div body))
	 (target2 (create-div body))
	 (object  (create-div target1)))

    (setf (positioning target1) :fixed)
    (setf (top target1) "10px")
    (setf (left target1) "10px")
    (setf (width target1) "100px")
    (setf (height target1) "100px")
    (setf (background-color target1) :yellow)
    
    (setf (positioning target2) :fixed)
    (setf (top target2) "10px")
    (setf (left target2) "140px")
    (setf (width target2) "100px")
    (setf (height target2) "100px")
    (setf (background-color target2) :yellow)
    
    (setf (positioning object) :absolute)
    (setf (top object) "10px")
    (setf (left object) "10px")
    (setf (width object) "50px")
    (setf (height object) "50px")
    (setf (background-color object) :green)

    ;; To allow for drag and drop requires:
    ;;
    ;; 1 object is draggable
    (setf (draggablep object) t)
    ;; 2 the on-drag-start event is set
    (set-on-drag-start object (lambda (obj)()) :drag-data "some data")

    ;; 4 the target on-drag-over event is sett
    (set-on-drag-over target1 (lambda (obj)()))
    ;; 5 the target on-drop event is set
    (set-on-drop target1 (lambda (obj data)
			   (place-inside-bottom-of target1 object)))
    
    (set-on-drag-over target2 (lambda (obj)()))
    (set-on-drop target2 (lambda (obj data)
			   (print (getf data :drag-data))
			   (place-inside-bottom-of target2 object)))
    
  (run body)))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
