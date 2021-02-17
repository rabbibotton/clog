(defpackage #:clog-tools
  (:use #:cl #:clog)
  (:export clog-db-admin))

(in-package :clog-tools)

(defclass app-data () ())

(defun on-new-window (body)
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app))
  (create-div body :content "Hello")
  (run body))

(defun clog-db-admin ()
  "Start clog-db-admin."
  (initialize #'on-new-window)
  (open-browser))
