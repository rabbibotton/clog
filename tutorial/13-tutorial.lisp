(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun start-tutorial ()
  "Start turtorial."
  (format t "Tutorial 13 is a how to on building your own clog appliction.~%~
             Copy the directory - ~A~%~
             to your ~~/common-lisp directory or other asdf / quicklisp~%~
             directory. Then follow the directions in the tuturial 13 ~%~
            directory."
	  (merge-pathnames "./tutorial/tutorial13"
			   (asdf:system-source-directory :clog))))
