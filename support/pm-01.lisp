;; mavis risetti PM on discord

(defpackage #:web
  (:use #:cl #:clog #:clog-gui)
  (:export start-web-app))

(in-package :web)

(defun on-new-window (body)
  (clog-gui-initialize body)
  (let ((ta (create-child body "<textarea>1</textarea>"))
        (gv (create-button body :content "Show me the value")))

    ;; This code will fail. The reason is that create-child by default
    ;; creates on object of type clog-element and value does not exist
    ;; clog-element
    (set-on-click gv (lambda (obj)
                       (alert-dialog body :content (value ta)))))

  (let ((ta (create-child body "<textarea>2</textarea>"
                          :clog-type 'clog:clog-form-element))
        (gv (create-button body :content "Show me the value")))

    ;; This code will work.
    (set-on-click gv (lambda (obj)
                       (alert-dialog body (value ta)))))

  (let ((ta (create-text-area body)) ;; Returns a clog-text-area
        (gv (create-button body :content "Show me the value")))

    ;; This code is the ideal as using a clog-text-area
    (set-on-click gv (lambda (obj)
                       (alert-dialog body (value ta)))))

  (run body))

(defun start-web-app ()
  "Start the App!"
  (initialize #'on-new-window)
  (open-browser))
