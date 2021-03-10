;;;; In this tutorial we are going to use clog-web for a local app, then
;;;; in the next tutorial we will redo the same app using an html "binding"
;;;; technique.
;;;;
;;;; --------------------------         -------
;;;; |ls -l                   |         | Run |
;;;; --------------------------         -------
;;;; ---------------------------------------------------------
;;;; | ls -l                                                 |
;;;; | total 434                                             |
;;;; | -rw------- 1 me user 246562 Dec  8 18:20 sample1.jpeg |
;;;; | -rw------- 1 me user 160290 Dec  8 18:21 sample2.jpeg |
;;;; |                                                       |
;;;; |                                                       |
;;;; |                                                       |
;;;; |                                                       |
;;;; |                                                       |
;;;; |                                                       |
;;;; ---------------------------------------------------------

(defpackage #:clog-user
  (:use #:cl #:clog #:clog-web)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (clog-web-initialize body)
  (setf (title (html-document body)) "Tutorial 25")
  ;; Setup two sections = command and result
  (let ((command-section (create-web-content body))
	(results-section (create-web-content body :class "w3-monospace")))
    ;; Setup command section
    (let* ((form    (create-form command-section))
	   (command (create-form-element form :text
					 :label (create-label form
						   :content "Enter Command: ")))
	   (button  (create-form-element form :submit)))
      (declare (ignore button))
      (set-on-submit form
	  (lambda (obj)
	    (declare (ignore obj))
	    (setf (inner-html results-section)
		  (format nil "~A<br>~A"
			  (inner-html results-section)
			  (lf-to-br (uiop/run-program:run-program
				     (value command)
				     :force-shell t :output :string))))
	    (setf (scroll-top results-section)
		  (scroll-height results-section))
	    (setf (value command) ""))))
    (setf (overflow results-section) :scroll)
    (set-border results-section :thin :solid :black)
    (setf (height results-section) (unit :px 500)))
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  (initialize #'on-new-window)
  (open-browser))
