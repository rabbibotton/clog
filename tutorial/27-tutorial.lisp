;; This tutorial demonstrates the panel box layout method

(defpackage #:clog-tut-27
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-27)

(defun on-new-window (body)
  (setf (title (html-document body)) "Tutorial 27") ;; set the page title
  (let* ((console (create-panel-box-layout body :left-width 200 :right-width 0))
         (head    (create-div (top-panel console) :content "Image Viewer"))
         (lbox    (create-select (left-panel console)))
         (viewer  (create-img (center-panel console)))
         (footer  (create-div (bottom-panel console)
                              :content "(c) 2021 David Botton - BSD 3 Lic.")))
    (declare (ignore footer))
    ;; Setup Top
    (setf (background-color (top-panel console)) :teal)
    (center-children (top-panel console))
    (setf (color head) :white)
    ;; Setup viewer
    (setf (width viewer) "100%")
    ;; Setup File List
    (setf (background-color (left-panel console)) :grey)
    (setf (positioning lbox) :absolute)
    (setf (size lbox) 2) ;; A size above 1 needed to get listbox
    (set-geometry lbox :left 0 :top 0 :bottom 0 :width 200)
    (add-select-options lbox '("kiarash-mansouri-fzoSNcxqtp8-unsplash.jpg"
                               "windmills.jpg"
                               "yellow-clogs.jpg"
                               "clogicon.png"))
    (flet ((img-changed (obj)
             (declare (ignore obj))
             (let* ((new-img (value lbox))
                    (new-src (format nil "/img/~A" new-img))
                    (new-url (format nil "~A//~A/?img=~A"
                                     (protocol (location body))
                                     (host     (location body))
                                     (quri:url-encode new-img))))
               ;; rewrite the url with selected img
               (url-rewrite (window body) new-url)
               ;; select img
               (setf (url-src viewer) new-src))))
      (set-on-change lbox #'img-changed)
      ;; On start up check if an image passed in url
      (let ((url-img (form-data-item (form-get-data body) "img")))
        (when url-img
          (setf (value lbox) url-img)
          (img-changed lbox))))
    ;; Setup Bottom
    (center-children (bottom-panel console) :horizontal nil)))

(defun start-tutorial ()
  (initialize 'on-new-window)
  (open-browser))
