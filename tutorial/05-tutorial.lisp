(defpackage #:clog-tut-5
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-5)

(defun my-on-click (obj)
  ;; Using connection-data-item it is possible to pass data that
  ;; is specific to an instance of a CLOG app. The connection
  ;; data items are accessible from every clog-object on the
  ;; same connection and are thread safe.
  (setf (color (connection-data-item obj "changer")) "green"))

(defun on-new-window (body)
  "On-new-window handler."  
  (setf (title (html-document body)) "Tutorial 5")
  (set-on-click (create-section body :h1 :content "Hello World! (click me!)")
		'my-on-click)
  (setf (connection-data-item body "changer")
	(create-section body :h1 :content "I change"))
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
