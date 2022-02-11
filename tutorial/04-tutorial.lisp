(defpackage #:clog-tut-4
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-4)

(defun my-on-click (obj)             ; obj in any event is the target of the event
  (setf (color obj) (rgb 0 255 0)))  ; this makes it possible to reuse event handlers
                                     ; RGB is a helper function for color
(defun on-new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Tutorial 4")
  ;; The same handler #'my-on-click is set on both targets
  (set-on-click (create-section body :h1 :content "Hello World! (click me!)")
		'my-on-click)
  (set-on-click (create-section body :h3 :content "Click me too!")
		'my-on-click)
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
