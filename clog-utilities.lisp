;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-system.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - CLOG Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; js-true-p ;;
;;;;;;;;;;;;;;;

(defun js-true-p (value)
  "Return true if VALUE equalp the string true"
  (equalp value "true"))

;;;;;;;;;;;;;;;
;; p-true-js ;;
;;;;;;;;;;;;;;;

(defun p-true-js (value)
  "Return \"true\" if VALUE true"
  (if value
      "true"
      "false"))

;;;;;;;;;;;;;;;;;;
;; open-browser ;;
;;;;;;;;;;;;;;;;;;

(defun open-browser (&key (url "http://127.0.0.1:8080"))
  "Open a web browser to URL."
  (trivial-open-browser:open-browser url))

;;;;;;;;;;;;;;;;;;;
;; escape-string ;;
;;;;;;;;;;;;;;;;;;;

(defun escape-string (str)
  "Escape STR for sending to browser script."
  (let ((res))
    (setf res (format nil "~A" str))
    (setf res (ppcre:regex-replace-all "\\x22" res "\\x22"))
    (setf res (ppcre:regex-replace-all "\\x27" res "\\x27"))
    (setf res (ppcre:regex-replace-all "\\x0A" res "\\x0A"))
    (setf res (ppcre:regex-replace-all "\\x0D" res "\\x0D"))
    res))
