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

(defclass clog-body (clog-obj)
  ((window
    :reader window
    :initarg :window)
   (document
    :reader html-document
    :initarg :document)
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
  (make-instance
   'clog-body
   :connection-id connection-id :html-id 0
   :window (make-instance 'clog-window    :connection-id connection-id)
   :window (make-instance 'clog-document  :connection-id connection-id)
   :window (make-instance 'clog-location  :connection-id connection-id)
   :window (make-instance 'clog-navigator :connection-id connection-id)))

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

