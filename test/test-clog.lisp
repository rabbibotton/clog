(defpackage #:test-clog
  (:use #:cl #:clog)
  (:export test))

(in-package :test-clog)

(defvar *last-obj*)

(defun on-new-window (win)
  (create-child win "<button>test</botton>")
  (create-child win "<H2>Cool!</H2>")
  (setf *last-obj* (create-child win "<button>a</button>")))

(defun test ()
  (print "Init connection")
  (initialize #'on-new-window :boot-file "/debug.html")
  (print "Connection set")
  (print "Open browser")
  (open-browser))
