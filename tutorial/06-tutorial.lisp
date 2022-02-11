(defpackage #:clog-tut-6
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-6)

(defun my-on-click (obj)
  (print "Event thread started")                    ; Every click will add a thread
  (unless (connection-data-item obj "isRunning")    ; So we toggle a connection-data-item
    (setf (connection-data-item obj "isRunning") t) ; in order to turn on and off the flashing.
    (setf (text obj) "(click me to stop!)")
    ;; When looping in an event or thread always check if the connection is still
    ;; valid to close down the event or thread.
    (loop
      (if (and (validp obj) (connection-data-item obj "isRunning"))
	  (progn
	    (setf (color obj) :green)
	    (sleep 0.3)
	    (setf (color obj) :red)
	    (sleep 0.3))
	  (return))))
  (setf (connection-data-item obj "isRunning") nil)
  (setf (text obj) "(click me to start!)")
  (setf (color obj) "black")
  (print "Event thread stopped"))

(defun on-new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Tutorial 6")
  (set-on-click (create-section body :h1 :content "(click me to start!)")
		'my-on-click)
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
