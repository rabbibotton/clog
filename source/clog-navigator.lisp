;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; system-clipboard-write ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric system-clipboard-write (clog-obj text)
  (:documentation "Write text to system clipboard"))

(defmethod system-clipboard-write ((obj clog-obj) text)
  (js-execute obj (format nil "navigator.clipboard.writeText('~A')"
                          (escape-string text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; system-clipboard-read ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric system-clipboard-read (clog-obj &key wait-timeout)
  (:documentation "Read text from system clipboard and return text."))

(defmethod system-clipboard-read ((obj clog-obj) &key (wait-timeout 1))
  (let ((doc (html-document (connection-body obj)))
        (sem (bordeaux-threads:make-semaphore))
        ret)
    (flet ((on-data (obj data)
             (declare (ignore obj))
             (bordeaux-threads:signal-semaphore sem)
             (setf ret data)))
      (set-on-event-with-data doc "on-clip-data" #'on-data :one-time t)
      (js-execute obj "navigator.clipboard.readText().then(function(text) {~
                        $(clog['document']).trigger('on-clip-data', text)})")
      (bordeaux-threads:wait-on-semaphore sem :timeout wait-timeout)
      ret)))
