(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (let* ((canvas (create-canvas body))
	 (cx     (create-context2d canvas)))
    )
  
  (run body))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
