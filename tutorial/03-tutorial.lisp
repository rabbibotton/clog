(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  "On-new-window handler."

  (setf (title (html-document body)) "Tutorial 3")
  
  (let ((hello-element
	  (create-section body :h1 :content "Hello World! (click me!)")))

    (let ((x 0))
      (set-on-click hello-element
		    (lambda (obj)
		      (declare (ignore obj))
		      (incf x)
		      (dotimes (n x)
			(create-p body
				  :content (format nil "Clicked ~A times." x))
			(sleep x)))))
    (run body)))

;; Running this version of the last tutorial and clicking quickly on the (click me!)
;; will demonstrate an important aspect of CLOG, events can happen in _parallel_.
;; This means that appropriate precautions to thread protect data should be taken
;; and that events do not wait for previous event handlers to complete.

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
