(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun my-on-click (obj)         ; obj in any event is the target of the event
  (setf (color obj) "green"))    ; this makes it possible to reuse events

(defun on-new-window (body)
  "On-new-window handler."

  (setf (title (html-document body)) "Tutorial 4")

  (set-on-click (create-child body "<h1>Hello World! (click me!)</h1>")
		#'my-on-click)

  (set-on-click (create-child body "<h3>Click me too!</h3>")
		#'my-on-click))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
