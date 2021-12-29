(defpackage #:clog-tut-13
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-13)

(defun start-tutorial ()
  "Start tutorial."
  (format t "Tutorial 13 is a how to on building your own clog application.~%~
             Copy the directory - ~A~%~
             to your ~~/common-lisp directory or other asdf / quicklisp~%~
             directory. Then follow the directions in the 13-tutorial/README.md ~%~
            directory."
	  (merge-pathnames "./tutorial/13-tutorial/hello-clog/"
			   (asdf:system-source-directory :clog))))
