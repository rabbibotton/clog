(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun my-on-click (obj)
  ;; Using connection-data-item it is possible to pass data that
  ;; is specific to an instance of a CLOG app. The connection
  ;; data items are accessible from every clog-object on the
  ;; same connection.
  (setf (color (connection-data-item obj "changer")) "green"))

(defun on-new-window (body)
  "On-new-window handler."
  
  (setf (title (html-document body)) "Tutorial 5")

  (set-on-click (create-child body "<h1>Hello World! (click me!)</h1>")
		#'my-on-click)

  (setf (connection-data-item body "changer")
	(create-child body "<h1>I change</h1>")))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
