(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-demo))

(in-package :clog-user)

(defun start-demo ()
  "How to allow only one CLOG app to be executed. Normally many browsers can
can be started with the same app."

  (clog:initialize
   (lambda (win)
     (let ((click-target (clog:create-child win "<h1>Hello World!</H1>")))
       (clog:set-on-click click-target
	(lambda ()
	  (clog:create-child win "<p>You Clicked me!</p>")))
       (clog:set-on-new-window
	(lambda (win)
	  (clog:put-br (clog:html-document win)
		       "Only single connection premitted."))))))

  (clog:open-browser))
