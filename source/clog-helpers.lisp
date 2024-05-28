;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-helpers.lisp                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Various helpers for CLOG

(cl:in-package :clog)

(defpackage #:clog-user
  (:use #:cl #:clog #:clog-gui #:clog-web)
  (:export :*body* :clog-repl))

(defpackage #:clog-tools
  (:use #:cl #:clog #:clog-gui #:clog-web)
  (:export :clog-builder
           :clog-open
           :add-supported-controls
           :control-info
           :clog-builder-probe
           :clog-db-admin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - CLOG Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar clog-user::*body* nil "clog-repl access to body")

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
  (funcall (symbol-function (find-symbol
                             "START-TUTORIAL"
                             (format nil "CLOG-TUT-~A" num)))))

;;;;;;;;;;;;;;;;;;;
;; load-tutorial ;;
;;;;;;;;;;;;;;;;;;;

(defun load-tutorial (num)
  "Load tutorial NUM - use (clog:run-tutorial)"
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
  (funcall (symbol-function (find-symbol
                             "START-DEMO"
                             (format nil "CLOG-DEMO-~A" num)))))

;;;;;;;;;;;;;;;
;; load-demo ;;
;;;;;;;;;;;;;;;

(defun load-demo (num)
  "Load demo NUM - use (clog:run-demo)"
  (let ((p (merge-pathnames (format nil "./demos/~2,'0d-demo.lisp" num)
                            (asdf:system-source-directory :clog))))
    (load p)
    (format t "~%~% ---- The demo src is located at: ~A~%" p)))

;;;;;;;;;;;;;;;
;; clog-repl ;;
;;;;;;;;;;;;;;;

(defun clog-repl (&key (clog-gui-initialize t)
		    (clog-web-initialize t)
		    (use-clog-debugger nil)
		    (boot-file "/debug.html")
		    (port 8080))
  "Set a path /repl that opens a blank page and sets the global
clog-user:*body* to last window openned to /repl. Debug mode is
set (logging to browser console) in the default debug.html boot-file.
clog-web and clog-gui are initialized and if use-clog-debugger it set to
true it is initialized and this repl window used as default clog debug display
and debugger display for clog events."
  (unless *clog-running*
    (initialize nil :boot-file boot-file :port port))
  (set-on-new-window (lambda (body)
                       (debug-mode body)
                       (when clog-web-initialize
                         (clog-web:clog-web-initialize body))
                       (when clog-gui-initialize
                         (clog-gui:clog-gui-initialize body :use-clog-debugger
                                                             use-clog-debugger))
                       (setf clog-user:*body* body)
                       (run body)
                       (when (eq clog-user:*body* body)
                         (setf clog-user:*body* nil)))
                     :path "/repl")
  (open-browser :url (format nil "http://127.0.0.1:~A/repl" *clog-port*))
  (format t "Use clog-user:*body* to access the clog-repl window."))

;;;;;;;;;;;;;;;;;;;;;;;
;; save-body-to-file ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun save-body-to-file (file-name &key (body clog-user::*body*)
                                      (if-exists :error)
                                      if-does-not-exist
                                      external-format)
  "Save the current html of BODY in the current state to FILE-NAME"
  (when (alexandria:write-string-into-file
         (outer-html (document-element (html-document body)))
         file-name
         :if-exists if-exists
         :if-does-not-exist if-does-not-exist
         :external-format external-format)
    t))
