(defpackage #:hello-clog
  (:use #:cl #:clog)
  (:export start-app))

(in-package :hello-clog)

(defun on-new-window (body)
  (create-div body :content "Hello World!")
  (run body))

(defun start-app ()
  (initialize 'on-new-window
	      :static-root (merge-pathnames "./www/"
			     (asdf:system-source-directory :hello-clog)))
  (open-browser))
