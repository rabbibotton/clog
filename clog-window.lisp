;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-window.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-window (clog-obj)()
  (:documentation "CLOG Window Objects encapsulate the window."))

;;;;;;;;;;;;;;;;;;;;;;
;; make-clog-window ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun make-clog-window (connection-id)
  "Construct a new clog-window. (Private)"
  (make-instance 'clog-window :connection-id connection-id :html-id "window"))

;;;;;;;;;;;;;;;;;
;; window-name ;;
;;;;;;;;;;;;;;;;;

(defgeneric window-name (clog-window)
  (:documentation "Get/Setf name for use by hyperlink \"target\" for this
window."))

(defmethod window-name ((obj clog-window))
  (property obj "name"))

(defgeneric set-window-name (clog-window value))
  
(defmethod set-window-name ((obj clog-window) value)
  (setf (property obj "name") value))
(defsetf window-name set-window-name)

;;;;;;;;;;;
;; alert ;;
;;;;;;;;;;;

(defgeneric alert (clog-window message)
  (:documentation "Launch an alert box. Note that as long as not dismissed
events and messages may not be trasmitted on most browsers."))

(defmethod alert ((obj clog-window) message)
  (cc:alert-box (connection-id obj) message))
