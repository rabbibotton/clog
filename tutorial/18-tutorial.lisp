(defpackage #:clog-tut-18
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-18)

;; Demonstrate drag and drop
(defun on-new-window (body)
  (let* ((target1 (create-div body))
	 (target2 (create-div body))
	 (object  (create-div target1))
	 (msg     (create-div body
		    :content "Drag green box to other yellow box")))
    ;; Instructions
    (setf (positioning msg) :fixed)
    (setf (top msg) "125px")
    ;; Box 1
    (setf (positioning target1) :fixed)
    (setf (top target1) "10px")
    (setf (left target1) "10px")
    (setf (width target1) "100px")
    (setf (height target1) "100px")
    (setf (background-color target1) :yellow)
    ;; Box 2
    (setf (positioning target2) :fixed)
    (setf (top target2) "10px")
    (setf (left target2) "140px")
    (setf (width target2) "100px")
    (setf (height target2) "100px")
    (setf (background-color target2) :yellow)
    ;; Box to Drag
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
    (set-on-drag-start object (lambda (obj)(declare (ignore obj))()) :drag-data "some data")
    ;; 4 the target on-drag-over event is set
    (set-on-drag-over target1 (lambda (obj)(declare (ignore obj))()))
    ;; 5 the target on-drop event is set
    (set-on-drop target1 (lambda (obj data)
			   (declare (ignore obj) (ignore data))
			   (place-inside-bottom-of target1 object)))
    ;; Set up other box 1 also as target for returning drag box
    (set-on-drag-over target2 (lambda (obj)(declare (ignore obj))()))
    (set-on-drop target2 (lambda (obj data)
			   (declare (ignore obj))
			   (print (getf data :drag-data))
			   (place-inside-bottom-of target2 object)))
  (run body)))

(defun start-tutorial ()
  "Start tutorial."
  (initialize 'on-new-window)
  (open-browser))
