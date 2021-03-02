;; In this tutorial we use clog-web to create a dynamic modern mobile
;; compatible web page. (In progress)

(defpackage #:clog-user
  (:use #:cl #:clog #:clog-web)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (clog-web-initialize body)
  (setf (title (html-document body)) "Tutorial 24")
  (create-web-panel body :content "<h3>Note:</h3><p>This is a Panel</p>" :class "w3-yellow")
  (create-section (create-web-content body :class "w3-teal")
		  :p :content "This is come content. I am centered and set to a maximum-width.")  
  ;; These containers not in a row and no setting for how main grid columns so are stacked
  (create-web-container body :content "Container 1" :class "w3-border")
  (create-web-container body :content "Container 2" :class "w3-border")
  (create-web-container body :content "Container 3" :class "w3-border")
  ;; These are in a row and each is a third for the 12 grid columns
  (let ((row (create-web-row body)))
    (create-web-container row :content "Container 1" :column-size :third :class "w3-border")
    (create-web-container row :content "Container 2" :column-size :third :class "w3-border")
    (create-web-container row :content "Container 3" :column-size :third :class "w3-border"))
  ;; As before with padding added between columns and some color
  (let ((row (create-web-row body :padding t)))
    (create-web-container row :content "Container 1" :column-size :third :class "w3-border w3-red")
    (create-web-container row :content "Container 2" :column-size :third :class "w3-border w3-orange")
    (create-web-container row :content "Container 3" :column-size :third :class "w3-border w3-blue"))
  (let ((row (create-web-auto-row body)))
    (create-web-auto-column row :content "Container 1<br>Container 1<br>Container 1"
				:vertical-align :middle :class "w3-border")
    (create-web-auto-column row :content "Container 2" :vertical-align :top :class "w3-border")
    (create-web-auto-column row :content "Container 3" :vertical-align :bottom :class "w3-border"))
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  (initialize #'on-new-window)
  (open-browser))
