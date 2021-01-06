(defpackage #:clog-user               ; Setup a package for our work to exist in
  (:use #:cl #:clog)                  ; Use the Common Lisp language and CLOG
  (:export start-tutorial))           ; Export as public the start-tutorial function

(in-package :clog-user)               ; Tell the "reader" we are in the clog-user package


;; Define our CLOG application

(defun on-new-window (win)                   ; define a function to be called 
  (create-child win "<h1>Hello World!<h1>"))
  


(defun start-tutorial ()   ; Define the function called start-tutorial
  "Start turtorial."       ; Optional docstring to describe function

  ;; Initialize the CLOG system
  (initialize #'on-new-window)
  ;; Set the function on-new-window to execute
  ;; everytime a browser connection to our app.
  ;; #' tells common lisp to pass the function
  ;; to intialize and not to execute it.


  ;; Open a browser to http://12.0.0.1:8080 - the default for CLOG apps
  (open-browser))
