;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-presentations.lisp                                               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;; clog-presentations - link Lisp classes to CLOG objects

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-presentations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-form-element-to-slot ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-form-element-to-slot (clog-obj object accessor &key transform)
  "Link changes to (value CLOG-OBJ) to (ACESSOR OBJECT)"
  `(link-form-element-to-object ,clog-obj (,accessor ,object) :transform ,transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-element-to-slot ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-element-to-slot (clog-obj object accessor &key transform)
  "Link changes to (text CLOG-OBJ) to (ACESSOR OBJECT)"
  `(link-element-to-object ,clog-obj (,accessor ,object) :transform ,transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-slot-to-form-element ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-slot-to-form-element (object accessor clog-obj)
  "Link changes to lisp (ACCESSOR OBJECT) to (value CLOG-OBJ). Only one
element can be bound at a time to a list object."
  `(defmethod (setf ,accessor) :after (new-value (obj (eql ,object)))
     (setf (value ,clog-obj) new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-slot-to-element ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-slot-to-element (object accessor clog-obj)
  "Link changes to lisp (ACCESSOR OBJECT) to (text CLOG-OBJ). Only one
element can be bound at a time to a list object."
  `(defmethod (setf ,accessor) :after (new-value (obj (eql ,object)))
     (setf (text ,clog-obj) new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-form-element-to-object ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-form-element-to-object (clog-obj object &key transform)
  "Link changes to (value CLOG-OBJ) to any lisp OBJECT"
  `(set-on-change ,clog-obj
		  (lambda (obj)
		    (declare (ignore obj))
		    (let ((v (if ,transform
				 (funcall ,transform (value ,clog-obj))
				 (value ,clog-obj))))
		      (setf ,object v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-element-to-object ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-element-to-object (clog-obj object &key transform)
  "Link changes to (text CLOG-OBJ) to any lisp OBJECT"
  `(set-on-change ,clog-obj
		  (lambda (obj)
		    (declare (ignore obj))
		    (let ((v (if ,transform
				 (funcall ,transform (text ,clog-obj))
				 (text ,clog-obj))))
		      (setf ,object v)))))
