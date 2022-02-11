(defpackage #:clog-tut-10
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-10)

;;; A very brief example of using the canvas control.
(defun on-new-window (body)
  (setf (title (html-document body)) "Tutorial 10")
  (let* ((canvas (create-canvas body :width 600 :height 400))
	 (cx     (create-context2d canvas)))
    (set-border canvas :thin :solid :black)    
    (fill-style cx :green)
    (fill-rect cx 10 10 150 100)
    (fill-style cx :blue)
    (font-style cx "bold 24px serif")
    (fill-text cx "Hello World" 10 150)
    (fill-style cx :red)
    (begin-path cx)
    (ellipse cx 200 200 50 7 0.78 0 6.29)
    (path-stroke cx)
    (path-fill cx)
    (run body)))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
