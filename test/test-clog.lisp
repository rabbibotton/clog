(defpackage #:test-clog
  (:use #:cl #:clog)
  (:export test on-connect))

(in-package :test-clog)

(defun on-connect (id)
  (place-after
   (place-after nil (create-with-html id "<button>test</botton>"))
   (create-with-html id "<H2>Cool!</H2>")))

(defun test ()
  (print "Init connection")
  (initialize #'on-connect :boot-file "/debug.html")
  (print "Open browser")
  (open-browser))
