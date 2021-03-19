(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  "On-new-window handler."
  ;; Give your app a name that appears in the browser tab/window
  (setf (title (html-document body)) "Tutorial 2")
  ;; The CLOG-body object gives you access to a number of other CLOG-Objects
  ;;
  ;; (html-document body) is the CLOG-Document object
  ;; (window body) is the CLOG-Window object
  ;; (location body) is the CLOG-Location object
  ;; (navigator body) is the CLOG-Navigator object
  
  (let ((hello-element	  
	  ;; CREATE-SECTION is a lispier way of creating any of the HTML 5
	  ;; section elements:
	  ;;
	  ;;   :address :article :aside :header :main :nav :hgroup
	  ;;   :p :pre :section :blockquote :h1 :h2 :h3 :h4 :h5 :h6
	  ;;
	  ;; Take a look at clog-element-common.lisp or the clog-manual	  
	  (create-section body :h1 :content "Hello World! (click me!)")))
    
    (let ((x 0))                     ; A closure - each call to on-new-window will
      (set-on-click hello-element    ; create a different version of this closer.
		    (lambda (obj)
		      (declare (ignore obj))
		      (incf x)
		      (dotimes (n x)
			(create-child body
				      (format nil "<p>Clicked ~A times.</p>" x))
			(scroll-to (window body) 0 (height body))))))
    (run body)))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
