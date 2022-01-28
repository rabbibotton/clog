(defpackage #:clog-tut-28
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-13)

(defun start-tutorial ()
  "Start tutorial."
  (format t "Tutorial 28 is a how to on building your own clog builder application.~%~
             Copy the directory - ~A~%~
             to your ~~/common-lisp directory or other asdf / quicklisp~%~
             directory. Then follow the directions in the 28-tutorial/README.md ~%~
            directory."
	  (merge-pathnames "./tutorial/28-tutorial/hello-builder/"
			   (asdf:system-source-directory :clog))))
