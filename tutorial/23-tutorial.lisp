(defpackage #:clog-tut-23
  (:use #:cl #:clog #:clog-gui)
  (:export start-tutorial))

(in-package :clog-tut-23)

;; This is a simple demo using semaphores to wait for user input
;; ask demonstrates the mechanics in general and the modal dialog
;; example show a more practical example.
(defun ask (obj)
  (let ((result nil)
	(hold (bordeaux-threads:make-semaphore))
	(q-box (create-div obj)))
    (set-on-click (create-button q-box :content "Yes")
		  (lambda (obj)
		    (declare (ignore obj))
		    (setf result :yes)
		    (bordeaux-threads:signal-semaphore hold)))
    (set-on-click (create-button q-box :content "No")
		  (lambda (obj)
		    (declare (ignore obj))
		    (setf result :no)
		    (bordeaux-threads:signal-semaphore hold)))
    (bordeaux-threads:wait-on-semaphore hold :timeout 10)
    (destroy q-box)
    result))

(defun on-new-window (body)
  ;; clog-gui can be mixed in to non-desktop environments, but
  ;; to use you must call first clog-gui-initialize
  (clog-gui-initialize body)
  (setf (title (html-document body)) "Tutorial 23")
  (set-on-click (create-button body :content
			       "Click for my question. You have 10 seconds to answer.")
		(lambda (obj)
		  (setf (disabledp obj) t)
		  ;; ask returns once an answer is given or times out
		  (create-div body :content (ask body))
		  ;; once ask returns with its answer (yes no or nil for timeout)
		  ;; the next statment is processed to open a dialog
		  (let ((hold (bordeaux-threads:make-semaphore)))
		    (confirm-dialog body "Are you sure?"
				    (lambda (answer)
				      (if answer
					  (create-div body :content "Great!")
					  (create-div body :content "Next time be sure!"))
				      (bordeaux-threads:signal-semaphore hold)))
		    (bordeaux-threads:wait-on-semaphore hold :timeout 60)
		    (create-div body :content "Thank you for answering!")))
		:one-time t)
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
