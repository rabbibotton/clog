;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-helpers.lisp  - Various helpers for learning                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

(defpackage #:clog-user
  (:use #:cl #:clog))

(defvar clog-user::*body* nil "clog-repl access to body")

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
  (load-tutorial num)
  (funcall (symbol-function (find-symbol "START-TUTORIAL" "CLOG-USER"))))

;;;;;;;;;;;;;;;;;;;
;; load-tutorial ;;
;;;;;;;;;;;;;;;;;;;

(defun load-tutorial (num)
  "Load tutorial NUM - use (clog-user:start-tutorial)"
  (let ((p (merge-pathnames (format nil "./tutorial/~2,'0d-tutorial.lisp" num)
			    (asdf:system-source-directory :clog))))
    (load p)
    (format t "~%~% ---- The tutorial src is located at: ~A~%" p)))

;;;;;;;;;;;;;;
;; run-demo ;;
;;;;;;;;;;;;;;

(defun run-demo (num)
  "Run demo NUM"
  (load-demo num)
  (funcall (symbol-function (find-symbol "START-DEMO" "CLOG-USER"))))

;;;;;;;;;;;;;;;
;; load-demo ;;
;;;;;;;;;;;;;;;

(defun load-demo (num)
  "Load demo NUM - use (clog-user:start-demo)"
  (let ((p (merge-pathnames (format nil "./demos/~2,'0d-demo.lisp" num)
			    (asdf:system-source-directory :clog))))
    (load p)
    (format t "~%~% ---- The demo src is located at: ~A~%" p)))

;;;;;;;;;;;;;;;
;; clog-repl ;;
;;;;;;;;;;;;;;;

(defun clog-repl ()
  "Set a path /repl that opens a blank page and sets the global
clog-user:*body* to last window openned to /repl."
  (unless *clog-running*
    (initialize nil :boot-file "/debug.html"))
  (set-on-new-window (lambda (body)(setf clog-user::*body* body))
		     :path "/repl")
  (open-browser :url "http://127.0.0.1:8080/repl")
  (format t "Use clog-user:*body* to access the clog-repl window."))
 
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
