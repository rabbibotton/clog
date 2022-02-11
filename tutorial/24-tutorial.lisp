;; In this tutorial we use clog-web to create a dynamic modern mobile
;; compatible web page using various clog-web containers.

(defpackage #:clog-tut-24
  (:use #:cl #:clog #:clog-web)
  (:export start-tutorial))

(in-package :clog-tut-24)

(defun on-new-window (body)
  (clog-web-initialize body)
  (setf (title (html-document body)) "Tutorial 24")
  (let ((side (create-web-sidebar body :class  "w3-animate-right"
				       :hidden t))
	(main (create-web-main body)))
    ;; Setup sidebar:
    (setf (right side) (unit :px 0))
    (add-card-look side)
    (set-on-click (create-web-sidebar-item side :content "Close &times;"
						:class   "w3-teal")
		  (lambda (obj)
		    (declare (ignore obj))		    
		    (setf (display side) :none)))
    (set-on-click (create-web-sidebar-item side :content "Google")
		  (lambda (obj)
		    (declare (ignore obj))		    
		    (setf (url (location body)) "http://google.com")))
    (create-web-sidebar-item side :content "item 2")
    (create-web-sidebar-item side :content "item 3")
    ;; Setup main content:
    (let* ((com (create-web-compositor main))
	   (img (create-img com    :url-src "/img/kiarash-mansouri-fzoSNcxqtp8-unsplash.jpg"))
	   (btn (create-button com :content "&#9776;"
				   :class "w3-button w3-text-white"))
	   (txt (create-div com    :content "CLOG - Beyond Web Frameworks!"
			           :class   "w3-center w3-text-white w3-cursive w3-xlarge"))
	   (txp (create-img com    :url-src "/img/clogwicon.png"))
	   (url (create-div com    :content "https://github.com/rabbibotton/clog"
			           :hidden  t
				   :class   "w3-text-white w3-large")))
      ;; composite main image
      (setf (box-width img) "100%")
      (setf (box-height img) "200")
      ;; composite top-right button to open sidebar
      (composite-top-right btn)
      (set-on-click btn
		  (lambda (obj)
		    (declare (ignore obj))
		    (setf (display side) :block)))      
      ;; composite middle text
      (composite-middle txt)
      ;; composite clog icon
      (composite-position txp :top 20 :left 20)
      (set-on-click txp (lambda (obj)
			  (declare (ignore obj))
			  (setf (url (location body)) "https://github.com/rabbibotton/clog")))
      (composite-top-middle url :padding-class :padding-32)
      (set-on-mouse-enter txp (lambda (obj)
				(declare (ignore obj))
				(setf (visiblep url) t)))
      (set-on-mouse-leave txp (lambda (obj)
				(declare (ignore obj))
				(setf (visiblep url) nil)))
      (composite-bottom-middle (create-div com :content "This is a 'compositor' container"
					       :class   "w3-text-white")))
    ;; Panels
    (create-web-panel main :content "<h3>Note:</h3><p>This is a 'panel' container</p>"
			   :class   "w3-yellow")
    (create-section (create-web-content main :class "w3-teal")
		    :p :content "This is a 'content' container.
                                 The container is centered and set to a maximum-width.")
    ;; Using containers and the 12 column grid
    (create-section (create-web-content main)
		    :p :content "Try and adjust size of browser to see reactions.<br>
                                 These are in a row container and each is a third of the 12 column grid")
    (let ((row (create-web-row main)))
      (create-web-container row :content "Grid Container 1" :column-size :third :class "w3-border")
      (create-web-container row :content "Grid Container 2" :column-size :third :class "w3-border")
      (create-web-container row :content "Grid Container 3" :column-size :third :class "w3-border"))
    ;; As before with padding added between columns and some color
    (create-section (create-web-content main)
		    :p :content "These are in a row container with padding turned on
                                 and each is a third of the 12 column grid")
    (let ((row (create-web-row main :padding t)))
      (create-web-container row :content "Grid Container 1" :column-size :third :class "w3-border w3-red")
      (create-web-container row :content "Grid Container 2" :column-size :third :class "w3-border w3-orange")
      (create-web-container row :content "Grid Container 3" :column-size :third :class "w3-border w3-blue"))
    ;; Using the auto layout that adjusts for content sizes automaticly
    (create-section (create-web-content main) :p :content "These are in an auto-row container")
    (let ((row (create-web-auto-row main)))
      (create-web-auto-column row :content "Auto Column 1<br>Auto Column 1<br>Auto Column 1"
				  :vertical-align :middle :class "w3-border")
      (create-web-auto-column row :content "Auto Column 2" :vertical-align :top :class "w3-border")
      (create-web-auto-column row :content "Auto Column 3" :vertical-align :bottom :class "w3-border"))
    ;; A "code" block
    (create-section (create-web-content main) :p :content "This a code block")
    (create-web-code main :content
		     ";; This is a code block<br>
                      (defun start-tutorial ()<br>
                      \"Start turtorial.\"<br>
                      (initialize 'on-new-window)<br>
                      (open-browser))")
    (run body)))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
