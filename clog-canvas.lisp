;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-canvas.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-canvas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-canvas (clog-element)()
  (:documentation "CLOG Canvas Objects."))


;;;;;;;;;;;;;;;;;;;
;; create-canvas ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-canvas (clog-obj &key auto-place)
  (:documentation "Create a new CLOG-Canvas as child of CLOG-OBJ if
:AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ."))

(defmethod create-canvas ((obj clog-obj) &key (auto-place t))
  (create-child obj "<canvas/>"
		:clog-type 'clog-canvas :auto-place auto-place))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-context2d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-context2d ()
  ((connection-id
    :reader connection-id
    :initarg :connection-id)
   (html-id
    :reader html-id
    :initarg :html-id))
  (:documentation "CLOG Context Objects."))

;;;;;;;;;;;;;;;;;;;;;;
;; create-context2d ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-context2d (clog-canvas)
  (:documentation "Create a new CLOG-Context2d from a CLOG-Canvas"))


(defmethod create-context2d ((obj clog-canvas))
  (let ((web-id (cc:generate-id)))
    (cc:execute (connection-id obj)
		(format nil "clog['~A']=clog['~A'].get(0).getContext('2d')"
			web-id
			(html-id obj)))
    
    (make-instance 'clog-context2d
		   :connection-id (connection-id obj)
		   :html-id web-id)))

