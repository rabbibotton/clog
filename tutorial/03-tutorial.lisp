(defpackage #:clog-tut-3
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-3)

(defun on-new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Tutorial 3")  
  (let ((hello-element
	  (create-section body :h1 :content "Hello World! (click me!)")))
    (let ((x 0))
      (set-on-click hello-element
		    (lambda (obj)
		      (declare (ignorable obj))
		      ;; Add to try non-parallel events:
		      ;; (with-sync-event (obj)
		      (let ((y (incf x)))
			(dotimes (n y)
			  (create-p body
				    :content (format nil "Clicked ~A times." y))
			  (sleep y)))))) ;)
    (run body)))

;;; Running this version of the last tutorial and clicking quickly on the (click me!)
;;; will demonstrate an important aspect of CLOG, events can happen in _parallel_.
;;; This means that appropriate precautions to thread protect data should be taken
;;; and that events do not wait for previous event handlers to complete. To change
;;; this behavior just add at start of event WITH-SYNC-EVENT and then all events
;;; will be serialized like in "traditional" GUIs to that event, events using
;;; WITH-SYNC-EVENT will be on same queue of incoming events and syncronized.
;;; But... notice what happens once syncing is on the next event doesn't hit until
;;; SLEEP returns.
(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
