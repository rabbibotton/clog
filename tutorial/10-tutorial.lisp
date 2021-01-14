(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (let* ((canvas (create-canvas body :width 600 :height 400))
	 (cx     (create-context2d canvas)))

    (set-border canvas :thin :solid :black)
    
    (clog::execute cx "fillStyle='green'")
    (clog::execute cx "fillRect(10, 10, 150, 100)"))
  
  (run body))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
