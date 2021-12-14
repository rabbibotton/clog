;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-window.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-body
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-body (clog-element)
  ((window
    :reader window
    :initarg :window)
   (html-document
    :reader html-document
    :initarg :html-document)
   (location
    :reader location
    :initarg :location)
   (navigator
    :reader navigator
    :initarg :navigator))
  (:documentation "CLOG Body Object encapsulate the view in the window."))

;;;;;;;;;;;;;;;;;;;;;;
;; make-clog-window ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun make-clog-body (connection-id)
  "Construct a new clog-body object."
  (let ((body (make-instance
	       'clog-body
	       :connection-id connection-id :html-id 0
	       :window        (make-clog-window    connection-id)
	       :html-document (make-clog-document  connection-id)
	       :location      (make-clog-location  connection-id)
	       :navigator     (make-clog-navigator connection-id))))
    (set-body (html-document body) body)))

;;;;;;;;;
;; run ;;
;;;;;;;;;

(defgeneric run (clog-body)
  (:documentation "Keeps the connection thread alive to prevent garbage
collection of local objects when not using connection-data objects
or global objects."))

(defmethod run ((obj clog-body))
  (loop
	(if (validp obj)
	    (sleep 10)
	    (return))))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-html-on-close ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-html-on-close (clog-body html)
  (:documentation "In case of connection loss to this CLOG-BODY, replace the browser contents
with HTML."))

(defmethod set-html-on-close ((obj clog-body) html)
  (clog-connection:set-html-on-close (connection-id obj) html))

;;;;;;;;;;;;
;; window ;;
;;;;;;;;;;;;

(defgeneric window (clog-body)
  (:documentation "Reader for CLOG-Window object"))

;;;;;;;;;;;;;;
;; document ;;
;;;;;;;;;;;;;;

(defgeneric html-document (clog-body)
  (:documentation "Reader for CLOG-Document object"))

;;;;;;;;;;;;;;
;; location ;;
;;;;;;;;;;;;;;

(defgeneric location (clog-body)
  (:documentation "Reader for CLOG-Location object"))

;;;;;;;;;;;;;;;
;; navigator ;;
;;;;;;;;;;;;;;;

(defgeneric navigator (clog-body)
  (:documentation "Reader for CLOG-Navigator object"))

