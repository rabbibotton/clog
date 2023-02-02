(mgl-pax:define-package :clog-callbacks
  (:documentation "The Common List Omnificent GUI - Callbacks")
  (:use #:cl #:mgl-pax)
  (:export
   :register-callback
   :find-callback
   :defcallback
   :call-callback
   :handle-callback
   :*callbacks*))

(in-package :clog-callbacks)

(defvar *callbacks* (make-hash-table :test 'equalp)
  "The table of callbacks.")

(defun register-callback (name handler)
  "Register HANDLER as a callback under NAME."
  (setf (gethash name *callbacks*) handler))

(defun find-callback (name &key (error-p t))
  "Find callback named with NAME."
  (or (gethash name *callbacks*)
      (and error-p
	   (error "Callback not defined: ~s" name))))

(defmacro defcallback (name args &body body)
  "Define a callback function."
  `(progn
     (defun ,name ,args ,@body)
     (register-callback ,(princ-to-string name) #',name)))

(defun call-callback (name args)
  "Returns the javascript code for calling a registered callback function.
ARGS is a javascript expression (string) that returns a JSON with the arguments for the callback."
  (format nil "ws.send('C:~a:' + ~a)" name args))

(defun handle-callback (name args)
  (let ((callback (find-callback name)))
    (funcall callback args)))
