(defpackage :todo-app-clog
  (:use :cl :clog))

(in-package :todo-app-clog)

(defun inc-count (obj)
  (print (connection-data-item obj "counter"))
  (incf (connection-data-item obj "counter")))

(defun dec-count (obj)
  (print (connection-data-item obj "counter"))
  (decf (connection-data-item obj "counter")))

(defun on-new-window (body)
  (setf (title (html-document body)) "Counter")
  (setf (connection-data-item body "counter") 0)
  (let* ((title (create-section body :h1 :content "Todo List:"))
         (inc-button (create-button body :content "Increase"))
         (dec-button (create-button body :content "Decrease"))
         (counter (create-section body :p :content (connection-data-item body "counter"))))
    (declare (ignorable title))
    (set-on-click inc-button (lambda (obj)
                               ;; this works as we update the counter
                               (funcall #'inc-count obj)
                               (setf (text counter) (connection-data-item body "counter"))))
    ;; you are not updating the couhter here
    (set-on-click dec-button #'dec-count)
    (run body)))

(defun run-app ()
  (initialize #'on-new-window)
  (open-browser))
