(mgl-pax:define-package :clog-callbacks
    (:documentation "The Common List Omnificent GUI - Callbacks")
  (:use #:cl #:mgl-pax)
  (:export
   :register-callback
   :find-callback
   :defcallback
   :call-callback
   :callback
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

(defun call-callback (name-or-lambda args)
  "Returns the javascript code for calling a registered callback function.
NAME-OR-LAMBDA can be either:
- A symbol. Should name a callback defined via DEFCALLBACK.
- A function. Probably a lambda. Gets registered as callback and called.
ARGS is a javascript expression (string) that returns a JSON with the arguments for the callback."
  (cond
    ((symbolp name-or-lambda)
     (format nil "ws.send('C:~a:' + ~a)" name-or-lambda args))
    ((functionp name-or-lambda)
     ;; assign a name and register the callback first
     (let ((callback-name (gensym "CALLBACK-")))
       (register-callback (princ-to-string callback-name) name-or-lambda)
       (format nil "ws.send('C:~a:' + ~a)" callback-name args)))
    (t (error "Invalid callback: ~s" name-or-lambda))))

(defun callback (name-or-lambda)
  "Create a callback. Returns a lambda expression that when applied to arguments return the javascript code that makes the call to the server callback.
NAME-OR-LAMBDA can be either:
- A symbol. Should name a callback defined via DEFCALLBACK.
- A function. Probably a lambda. Gets registered as callback and called."
  (cond
    ((symbolp name-or-lambda)
     (lambda (args)
       (format nil "ws.send('C:~a:' + ~a)" name-or-lambda args)))
    ((functionp name-or-lambda)
     ;; assign a name and register the callback first
     (let ((callback-name (gensym "CALLBACK-")))
       (register-callback (princ-to-string callback-name) name-or-lambda)
       (lambda (args)
         (format nil "ws.send('C:~a:' + ~a)" callback-name args))))
    (t (error "Invalid callback: ~s" name-or-lambda))))

(defun handle-callback (name args)
  (let ((callback (find-callback name)))
    (funcall callback args)))
