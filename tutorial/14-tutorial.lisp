(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (set-on-click (create-button body :content "Set Local Key")
		(lambda (obj)
		  (declare (ignore obj))
		  (setf (storage-element (window body) :local "my-local-key")
			(get-universal-time))
		  (reload (location body))))
  (set-on-click (create-button body :content "Set Session Key")
		(lambda (obj)
		  (declare (ignore obj))
		  (setf (storage-element (window body) :session "my-session-key")
			(get-universal-time))
		  (reload (location body))))
  (set-on-storage (window body)
		  (lambda (obj data)
		    (declare (ignore obj))
		    (create-div body :content
			    (format nil "<br>~A : ~A => ~A<br>"
				    (getf data ':key)
				    (getf data ':old-value)
				    (getf data ':value)))))
  (create-div body :content (format nil
       "<H1>Local Storage vs Session Storage</H1>
<p width=500>
The value of local storage persists in browser cache even after browser closed.
If you reset this page the session storage key will remain the same, but opening
in another window or tab will be a new session, but if it came from a click from
this window the session keys are copied first to the new window.</p>
<br>
<a href='.' target='_blank'>Another Window = Different Session</a><br>
<br>
<br>
Local Storage key: ~A := ~A<br>
<br>
Session Storage key: ~A := ~A<br>
<br>
Changes made to a local key will fire an event and print below:<br>"
       "my-local-key"
       (storage-element (window body) :local "my-local-key")
       "my-session-key"
       (storage-element (window body) :session "my-session-key")))
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  (initialize #'on-new-window)
  (open-browser))
