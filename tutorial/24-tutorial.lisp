;; In this tutorial we use clog-web to create a dynamic modern mobile
;; compatible web page. (In progress)

(defpackage #:clog-user
  (:use #:cl #:clog #:clog-web)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (clog-web-initialize body)
  (setf (title (html-document body)) "Tutorial 24")
  (let ((side (create-web-sidebar body :class "w3-animate-left w3-card"))
	(main (create-web-main body)))
    ;; Setup sidebar:
    (setf (display side) :none)
    (set-on-click (create-web-sidebar-item side :content "Close &times;"
						:class "w3-teal")
		  (lambda (obj)
		    (setf (display side) :none)))
    (set-on-click (create-web-sidebar-item side :content "Google")
		  (lambda (obj)
		    (setf (url (location body)) "http://google.com")))
    (create-web-sidebar-item side :content "item 2")
    (create-web-sidebar-item side :content "item 3")
    ;; Setup main content:
    ;;
    ;; Button to open sidebar
    (set-on-click (create-button main :content "&#9776;" :class "w3-button")
		  (lambda (obj)
		    (setf (display side) :block)))
    ;; Panels
    (create-web-panel main :content "<h3>Note:</h3><p>This is a Panel</p>" :class "w3-yellow")
    (create-section (create-web-content main :class "w3-teal")
		    :p :content "This is come content. I am centered and set to a maximum-width.")
    ;; Using containers and the 12 column grid
    ;; These containers not in a row and no setting for how main grid columns so are stacked
    (create-web-container main :content "Container 1" :class "w3-border")
    (create-web-container main :content "Container 2" :class "w3-border")
    (create-web-container main :content "Container 3" :class "w3-border")
    ;; These are in a row and each is a third of the 12 column grid
    (let ((row (create-web-row main)))
      (create-web-container row :content "Container 1" :column-size :third :class "w3-border")
      (create-web-container row :content "Container 2" :column-size :third :class "w3-border")
      (create-web-container row :content "Container 3" :column-size :third :class "w3-border"))
    ;; As before with padding added between columns and some color
    (let ((row (create-web-row main :padding t)))
      (create-web-container row :content "Container 1" :column-size :third :class "w3-border w3-red")
      (create-web-container row :content "Container 2" :column-size :third :class "w3-border w3-orange")
      (create-web-container row :content "Container 3" :column-size :third :class "w3-border w3-blue"))
    ;; Using the auto layout that adjusts for content sizes automaticly
    (let ((row (create-web-auto-row main)))
      (create-web-auto-column row :content "Container 1<br>Container 1<br>Container 1"
				  :vertical-align :middle :class "w3-border")
      (create-web-auto-column row :content "Container 2" :vertical-align :top :class "w3-border")
      (create-web-auto-column row :content "Container 3" :vertical-align :bottom :class "w3-border"))
    ;; A "code" block
    (create-web-code main :content
		     "(defun start-tutorial ()<br>
                      \"Start turtorial.\"<br>
                      (initialize #'on-new-window)<br>
                      (open-browser))")
    (run body)))

(defun start-tutorial ()
  "Start turtorial."
  (initialize #'on-new-window)
  (open-browser))
