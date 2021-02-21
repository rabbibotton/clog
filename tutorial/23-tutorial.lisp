(defpackage #:clog-user
  (:use #:cl #:clog #:clog-gui)
  (:export start-tutorial))

(in-package :clog-user)

;; This is a simple demo using semaphores to wait for user input
(defun ask (obj)
  (let ((result nil)
	(hold (bordeaux-threads:make-semaphore))
	(q-box (create-div obj)))
    (set-on-click (create-button q-box :content "Yes")
		  (lambda (obj)
		    (setf result :yes)
		    (bordeaux-threads:signal-semaphore hold)))
    (set-on-click (create-button q-box :content "No")
		  (lambda (obj)
		    (setf result :no)
		    (bordeaux-threads:signal-semaphore hold)))
    (bordeaux-threads:wait-on-semaphore hold :timeout 10)
    (destroy q-box)
    result))

(defun on-new-window (body)
  (set-on-click (create-button body :content
			       "Click for my question. You have 10 seconds to answer.")
		(lambda (obj)
		  (create-div body :content (ask body))))
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  (initialize #'on-new-window)
  (open-browser))
