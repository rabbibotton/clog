;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-presentations.lisp                                               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;; clog-presentations - link Lisp objects and CLOG objects

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-presentations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-slot-and-form-element ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-slot-and-form-element (object accessor clog-obj
				      &key (set-event #'set-on-change)
					transform-to-lisp
					transform-to-element)
  "Biderectional link slot (ACCESSOR OBJECT) <> clog-form-element (CLOG-OBJ)"
  `(progn
     (link-form-element-to-slot ,clog-obj ,object ,accessor
				:set-event ,set-event
				:transform ,transform-to-lisp)
     (link-slot-to-form-element ,object ,accessor ,clog-obj
				:transform ,transform-to-element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-slot-and-element ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-slot-and-element (object accessor clog-obj
				 &key (set-event #'set-on-change)
				   transform-to-lisp
				   transform-to-element)
  "Biderectional link slot (ACCESSOR OBJECT) <> clog-element (CLOG-OBJ)"
  `(progn
     (link-element-to-slot ,clog-obj ,object ,accessor
			   :set-event ,set-event
			   :transform ,transform-to-lisp)
     (link-slot-to-element ,object ,accessor ,clog-obj
			   :transform ,transform-to-element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-form-element-to-slot ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-form-element-to-slot (clog-obj object accessor
				     &key (set-event #'set-on-change)
				       transform)
  "Link changes to (value CLOG-OBJ) to (ACESSOR OBJECT)
on SET-EVENT with TRANSFORM"
  `(link-element-to-place ,clog-obj value (,accessor ,object)
			  :set-event ,set-event
			  :transform ,transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-element-to-slot ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-element-to-slot (clog-obj object accessor
				&key (set-event #'set-on-change)
				  transform)
  "Link changes to (text CLOG-OBJ) to (ACESSOR OBJECT)
on SET-EVENT with TRANSFORM"
  `(link-element-to-place ,clog-obj text (,accessor ,object)
				   :set-event ,set-event
				   :transform ,transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-element-to-place ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-element-to-place (clog-obj property place
				  &key (set-event #'set-on-change)
				    transform)
  "Link changes to (PROPERTY CLOG-OBJ) to any lisp PLACE
on SET-EVENT with TRANSFORM"
  `(funcall ,set-event ,clog-obj
	    (lambda (obj)
	      (declare (ignore obj))
	      (let ((v (if ,transform
			   (funcall ,transform (,property ,clog-obj))
			   (,property ,clog-obj))))
		(setf ,place v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-slot-to-form-element ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-slot-to-form-element (object accessor clog-obj &key transform)
  "Link changes to lisp (ACCESSOR OBJECT) to (value CLOG-OBJ). Only one
element can be bound at a time to a lisp object."
  `(link-slot-to-place ,object ,accessor (value ,clog-obj) :transform ,transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-slot-to-element ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-slot-to-element (object accessor clog-obj &key transform)
  "Link changes to lisp (ACCESSOR OBJECT) to (text CLOG-OBJ). Only one
element can be bound at a time to a lisp object."
  `(link-slot-to-place ,object ,accessor (text ,clog-obj) :transform ,transform))

;;;;;;;;;;;;;;;;;;;;;;;;
;; link-slot-to-place ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-slot-to-place (object accessor place &key transform)
  "Link changes to lisp (ACCESSOR OBJECT) to PLACE. Only one
PLACE can be bound at a time to a lisp object."
  `(defmethod (setf ,accessor) :after (new-value (obj (eql ,object)))
     (setf ,place (if ,transform
		      (funcall ,transform new-value)
		      new-value))))
