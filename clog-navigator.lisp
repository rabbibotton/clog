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

;;;;;;;;;;;;;;;;;;;;
;; cookie-enabled ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric cookie-enabled (clog-location)
  (:documentation "Get if cookie enabled."))

(defmethod cookie-enabled ((obj clog-location))
  (js-true-p (query obj "cookieEnabled")))

;;;;;;;;;;;;;;
;; language ;;
;;;;;;;;;;;;;;

(defgeneric language (clog-location)
  (:documentation "Get user prefered language."))

(defmethod language ((obj clog-location))
  (query obj "language"))

;;;;;;;;;;;;;;;;
;; user-agent ;;
;;;;;;;;;;;;;;;;

(defgeneric user-agent (clog-location)
  (:documentation "Get user agent."))

(defmethod user-agent ((obj clog-location))
  (query obj "userAgent"))

;;;;;;;;;;;;
;; vendor ;;
;;;;;;;;;;;;

(defgeneric vendor (clog-location)
  (:documentation "Get browser vendor."))

(defmethod vendor ((obj clog-location))
  (query obj "vendor"))
