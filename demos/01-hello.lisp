(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-demo))

(in-package :clog-user)

(defun start-demo ()
  "Simple Hello world using CLOG."

  ;; Initialize the CLOG system
  (clog:initialize
   (lambda (win)
     (clog:set-on-click
      (clog:create-child win "<h1>Hello World!</H1>")
      (lambda ()
	(clog:create-child win "<p>You Clicked me!</p>")))))

  (clog:open-browser))
