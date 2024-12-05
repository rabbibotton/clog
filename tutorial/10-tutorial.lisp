(defpackage #:clog-tut-10
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-10)

;;; A very brief example of using the canvas control.
(defun on-new-window (body)
  (setf (title (html-document body)) "Tutorial 10")
  (let* ((canvas (create-canvas body :width 600 :height 400))
         (cx     (create-context2d canvas))
         dat)
    (set-border canvas :thin :solid :black)
    (setf (fill-style cx) :green)
    (fill-rect cx 10 10 150 100)
    (setf (fill-style cx) :blue
          (font-style cx) "bold 24px serif")
    (fill-text cx "Hello World" 10 150)
    (begin-path cx)
    (let ((gr (create-linear-gradient cx 20 0 220 0)))
      (add-color-stop gr 0 :red)
      (add-color-stop gr .5 :cyan)
      (add-color-stop gr 1 :yellow)
      (setf (fill-style cx) gr))
    (ellipse cx 200 200 50 7 0.78 0 6.29)
    (path-stroke cx)
    (path-fill cx)
    (setf dat (get-image-data cx 100 100 10 10))
    (put-image-data cx dat 10 200)
    (let ((data (json-image-data dat)))
      (setf data (ppcre:regex-replace-all ":0" data ":255"))
      (setf (json-image-data dat) data))
    (put-image-data cx dat 30 200)))

(defun start-tutorial ()
  "Start tutorial."
  (initialize 'on-new-window)
  (open-browser))
