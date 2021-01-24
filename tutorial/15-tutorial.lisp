(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (create-video body :source "https://www.w3schools.com/html/mov_bbb.mp4")
  (create-audio body :source "https://www.w3schools.com/html/horse.ogg")
  
  (run body))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
