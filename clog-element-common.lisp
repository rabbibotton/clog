;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-element-commont.lisp                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-br
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-br (clog-element)()
  (:documentation "CLOG BR Objects for line breaks."))

;;;;;;;;;;;;;;;
;; create-br ;;
;;;;;;;;;;;;;;;

(defgeneric create-br (clog-obj &key content auto-place)
  (:documentation "Create a new CLOG-BR as child of CLOG-OBJ that creates a
line break and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-br ((obj clog-obj) &key (content "") (auto-place t))
  (create-child obj (format nil "<br />" (escape-string content))
		:clog-type 'clog-br))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-div
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-div (clog-element)()
  (:documentation "CLOG Div Objects."))

;;;;;;;;;;;;;;;;
;; create-div ;;
;;;;;;;;;;;;;;;;

(defgeneric create-div (clog-obj &key content auto-place)
  (:documentation "Create a new CLOG-Div as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-div ((obj clog-obj) &key (content "") (auto-place t))
  (create-child obj (format nil "<div>~A</div>" (escape-string content))
		:clog-type 'clog-div))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-hr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-hr (clog-element)()
  (:documentation "CLOG HR Objects for horizontal rules."))

;;;;;;;;;;;;;;;
;; create-hr ;;
;;;;;;;;;;;;;;;

(defgeneric create-hr (clog-obj &key content auto-place)
  (:documentation "Create a new CLOG-HR as child of CLOG-OBJ that creates a
horizontal rule (line) and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-hr ((obj clog-obj) &key (content "") (auto-place t))
  (create-child obj (format nil "<hr />" (escape-string content))
		:clog-type 'clog-hr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-p
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-p (clog-element)()
  (:documentation "CLOG P Objects."))

;;;;;;;;;;;;;;
;; create-p ;;
;;;;;;;;;;;;;;

(defgeneric create-p (clog-obj &key content auto-place)
  (:documentation "Create a new CLOG-P as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-p ((obj clog-obj) &key (content "") (auto-place t))
  (create-child obj (format nil "<p>~A</p>" (escape-string content))
		:clog-type 'clog-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-span
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-span (clog-element)()
  (:documentation "CLOG Span Objects."))

;;;;;;;;;;;;;;;;;
;; create-span ;;
;;;;;;;;;;;;;;;;;

(defgeneric create-span (clog-obj &key content auto-place)
  (:documentation "Create a new CLOG-Span as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-span ((obj clog-obj) &key (content "") (auto-place t))
  (create-child obj (format nil "<span>~A</span>" (escape-string content))
		:clog-type 'clog-span))

