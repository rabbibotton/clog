;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-helpers.lisp  - Various helpers for learning                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - CLOG Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;
;; clog-install-dir ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun clog-install-dir ()
  "Return the directory CLOG was installed in."
  (asdf:system-source-directory :clog))

;;;;;;;;;;;;;;;;;
;; open-manual ;;
;;;;;;;;;;;;;;;;;

(defun open-manual ()
  "Launches a browser with CLOG manual."
  (open-browser :url (format nil "~A"
			     (merge-pathnames "./doc/clog-manual.html"
				      (asdf:system-source-directory :clog)))))
   
;;;;;;;;;;;;;;;;;;
;; run-tutorial ;;
;;;;;;;;;;;;;;;;;;

(defun run-tutorial (num)
  "Run tutorial NUM"
  (let ((p (merge-pathnames (format nil "./tutorial/~2,'0d-tutorial.lisp" num)
			    (asdf:system-source-directory :clog))))
    (load p)
    (clog-user:start-tutorial)
    (format t "~%~% ---- The tutorial src is located at: ~A~%" p)))

;;;;;;;;;;;;;;;;
;; load-world ;;
;;;;;;;;;;;;;;;;

(defun load-world ()
  (load "clog.lisp")
  (load "clog-docs.lisp")
  (load "clog-base.lisp")
  (load "clog-element.lisp")
  (load "clog-element-common.lisp")
  (load "clog-canvas.lisp")
  (load "clog-form.lisp")
  (load "clog-window.lisp")
  (load "clog-navigator.lisp")
  (load "clog-document.lisp")
  (load "clog-location.lisp")
  (load "clog-system.lisp")
  (load "clog-utilities.lisp")
  (load "clog-body.lisp")
  (load "clog-helpers.lisp"))

;;;;;;;;;;;;;;;;;;;;
;; make-mark-down ;;
;;;;;;;;;;;;;;;;;;;;

(defun make-mark-down ()
  (load-world)
  (describe clog:@CLOG-MANUAL))

;;;;;;;;;;;;;;;
;; make-html ;;
;;;;;;;;;;;;;;;

(defun make-html ()
  (load-world)
  (mgl-pax:update-asdf-system-html-docs clog:@CLOG-MANUAL :clog))

;;;;;;;;;;;;;;;;
;; make-world ;;
;;;;;;;;;;;;;;;;

(defun make-world ()
  (make-html)
  (asdf:compile-system :clog))
