(defpackage #:clog-tut-1              ; Setup a package for our work to exist in
  (:use #:cl #:clog)                  ; Use the Common Lisp language and CLOG
  (:export start-tutorial))           ; Export as public the start-tutorial function

(in-package :clog-tut-1)              ; Tell the "reader" we are in the clog-tut-1 package


;; Define our CLOG application
(defun on-new-window (body)           ; Define the function called on-new-window
  "On-new-window handler."            ; Optional docstring to describe function

  (let ((hello-element                ; hello-element is a local variable that
	                              ; will be bound to our new CLOG-Element
	  
	  ;; This application simply creates a CLOG-Element as a child to the
	  ;; CLOG-body object in the browser window.
	  
	  ;; A CLOG-Element represents a block of HTML (we will see later ways to
	  ;; directly create buttons and all sorts of HTML elements in more lisp
	  ;; like ways with no knowledge of HTML or javascript. CREATE-CHILD
	  ;; allows any html element to be created and returned as a CLOG-Element.
	  (create-child body "<h1>Hello World! (click me!)</h1>")))

    (set-on-click hello-element      ; Now we set a function to handle clicks
		  (lambda (obj)      ; In this case we use an anonymous function
		    (declare (ignore obj))
		    (setf (color hello-element) :green)))
    
    (run body))) ; Keep our thread alive until connection closes
		 ; and prevent garbage collection of our CLOG-Objects
		 ; until no longer needed.

;;; To see all the events one can set and the many properties and styles that
;;; exist, take a look through the CLOG manual or the file clog-element.lisp

(defun start-tutorial ()   ; Define the function called start-tutorial
  "Start turtorial."       ; Optional docstring to describe function

  ;; Initialize the CLOG system
  (initialize #'on-new-window)
  ;; Set the function on-new-window to execute
  ;; everytime a browser connection to our app.
  ;; #' tells common lisp to pass the function.
  ;; If we pass the symbol 'on-new-window it
  ;; it will look each time for the function
  ;; represented by our symbol. This is generally
  ;; prefered in development as then we can
  ;; recompile our events while running ie. M-C-x
  ;; in emacs.

  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (open-browser))
