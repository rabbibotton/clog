(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  "On-new-window handler."

  (setf (title (html-document body)) "Tutorial 2")
  
  (let ((hello-element
	  (create-child body "<h1>Hello World! (click me!)</h1>")))

    (let ((x 0))
      (set-on-click hello-element
		    (lambda ()
		      (incf x)
		      (dotimes (n x)
			(create-child body
				      (format nil "<p>Clicked ~A times.</p>" x))
			(sleep x)))))))

;; Running this version of the last tutorial and clicking quickly on the (click me!)
;; will demonstrate an important aspect of CLOG, events can happen in _parallel_.
;; This means that appropriate precautions to thread protect data should be taken
;; and that events do not wait for previous event handlers to complete.

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
