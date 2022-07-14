;; Checking the color of a CLOG object. #38
;; Kaeland Chatman



(defpackage #:web
  (:use #:cl #:clog)
  (:export start-web-app))

(in-package :web)

(defun my-on-click (obj)
  (let ((obj-color (color (connection-data-item obj "changer"))))
    ;; Although the "obj-color" is a string, it is the
    ;; RGB value of the color property. I was expecting it to be the value
    ;; I set it to: "purple". Is there a way to
    ;; conditionally match the color in the if-statement below?
    ;; Or, is there a CLOG color util to help find the rgb value of a
    ;; CLOG object? This would help with matching in the if-statment.

    ;; DB: The answer is to use the rgb function. Although I would keep
    ;;     state in lisp and not worry about the browser values.

    (print obj-color)
    ;;    (if (string-equal obj-color "purple")
    (if (string-equal obj-color (rgb 128 0 128)) ; you will need to use the rgb for purple
        (setf (color (connection-data-item obj "changer")) "black")
        (setf (color (connection-data-item obj "changer")) "purple"))))

(defun on-new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "A simple App")
  (set-on-click (create-section body :h1 :content "Purple-o-nator")
                #'my-on-click)
  (setf (connection-data-item body "changer")
        (create-section body :h1 :content "Am I purple, yet?!"))
  (run body))

(defun start-web-app ()
  "Start the App!"
  (initialize #'on-new-window)
  (open-browser))
