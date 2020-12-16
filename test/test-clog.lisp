(defpackage #:test-clog
  (:use #:cl #:clog)
  (:export test on-connect))

(in-package :test-clog)

(defun on-connect (id)
  (alert-box id "We are here"))

(defun test ()
  (print "Init connection")
  (initialize #'on-connect :boot-file "/debug.html")
  (print "Open browser")
  (open-browser)
)
