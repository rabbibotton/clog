(defpackage #:hello-builder
  (:use #:cl #:clog)
  (:export start-app))

(in-package :hello-builder)

(defun my-click (panel)
  (setf (font (hello-span panel)) (format nil "~Apx Times, serif" (random 36)))
  (setf (color (hello-span panel)) (rgb (random 255) (random 255) (random 255))))

(defun start-app ()
  (initialize 'create-hello-page
	      :static-root (merge-pathnames "./www/"
			     (asdf:system-source-directory :hello-builder)))
  (open-browser))
