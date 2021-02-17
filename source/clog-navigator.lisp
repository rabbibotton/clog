;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-navigator.lisp                                                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-navigator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-navigator (clog-obj)()
  (:documentation "CLOG Navigator Objects encapsulate the navigator."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-clog-navigator ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-clog-navigator (connection-id)
  "Construct a new clog-navigator. (Private)"
  (make-instance 'clog-navigator :connection-id connection-id :html-id "navigator"))

;;;;;;;;;;;;;;;;;;;;;;
;; cookie-enabled-p ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric cookie-enabled-p (clog-navigator)
  (:documentation "Get if cookie enabled."))

(defmethod cookie-enabled-p ((obj clog-navigator))
  (js-true-p (query obj "cookieEnabled")))

;;;;;;;;;;;;;;
;; language ;;
;;;;;;;;;;;;;;

(defgeneric language (clog-navigator)
  (:documentation "Get user prefered language."))

(defmethod language ((obj clog-navigator))
  (query obj "language"))

;;;;;;;;;;;;;;;;
;; user-agent ;;
;;;;;;;;;;;;;;;;

(defgeneric user-agent (clog-navigator)
  (:documentation "Get user agent."))

(defmethod user-agent ((obj clog-navigator))
  (query obj "userAgent"))

;;;;;;;;;;;;
;; vendor ;;
;;;;;;;;;;;;

(defgeneric vendor (clog-navigator)
  (:documentation "Get browser vendor."))

(defmethod vendor ((obj clog-navigator))
  (query obj "vendor"))
