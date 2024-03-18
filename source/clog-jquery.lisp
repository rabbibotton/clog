;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-jquery.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;; clog-jquery - use the jQuery queries form with in clog

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-jquery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-jquery (clog-element)()
  (:documentation "CLOG jQuery objects. A jQuery representa DOM queries that
can represent one or even many CLOG objects as a single element."))

;;;;;;;;;;;;;;;;;;;
;; create-jquery ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-jquery (clog-obj jquery)
  (:documentation "Create a new CLOG-JQUERY from JQUERY.
Some sample jquery selectors:
    *
    .class
    element_name
    #id
    selector1, selectorN, ..."))

(defmethod create-jquery ((obj clog-obj) jquery)
  (let ((html-id (format nil "CLOG~A" (generate-id))))
    (js-execute obj
                (format nil
                        "clog['~A']=$(\"~A\")"
                        html-id jquery))
    (make-clog-element (connection-id obj) html-id :clog-type 'clog-jquery)))

;;;;;;;;;;;;
;; jquery ;;
;;;;;;;;;;;;

(defgeneric jquery (clog-obj)
  (:documentation "Return the jQuery accessor for and CLOG-OBJ."))

(defmethod jquery ((obj clog-obj))
  (format nil "$(~A)" (script-id obj)))

;;;;;;;;;;;;;;;;;;;;
;; jquery-execute ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric jquery-execute (clog-obj method)
  (:documentation "Execute the jQuery METHOD on and CLOG-OBJ. Result is
dicarded, returns CLOG-OBJ."))

(defmethod jquery-execute ((obj clog-obj) method)
  (js-execute obj (format nil "~A.~A" (jquery obj) method)))

;;;;;;;;;;;;;;;;;;
;; jquery-query ;;
;;;;;;;;;;;;;;;;;;

(defgeneric jquery-query (clog-obj method &key default-answer)
  (:documentation "Execute the jQuery METHOD on ANY CLOG-OBJ and return
result or DEFAULT-ANSWER on time out."))

(defmethod jquery-query ((obj clog-obj) method &key (default-answer nil))
  (js-query obj (format nil "~A.~A" (jquery obj) method)
            :default-answer default-answer))
