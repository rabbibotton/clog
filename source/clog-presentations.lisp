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

(defmacro link-form-element-to-slot (clog-obj object accessor)
  "Link changes to (value CLOG-OBJ) to (ACESSOR OBJECT)"
  `(set-on-change ,clog-obj
		  (lambda (obj)
		    (declare (ignore obj))
		    (setf (,slot-name ,object) (value ,clog-obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-element-to-slot ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-element-to-slot (clog-obj object slot-name)
  "Link changes to (text CLOG-OBJ) to (ACESSOR OBJECT)"
  `(set-on-change ,clog-obj
		  (lambda (obj)
		    (declare (ignore obj))
		    (setf (,slot-name ,object) (text ,clog-obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-slot-to-form-element ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-slot-to-form-element (object accessor clog-obj)
  "Link changes to lisp (ACCESSOR OBJECT) to (value CLOG-OBJ)"
  `(defmethod (setf ,accessor) :after (new-value (obj (eql ,object)))
     (setf (value ,clog-obj) new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link-slot-to-element ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro link-slot-to-element (object accessor clog-obj)
  "Link changes to lisp (ACCESSOR OBJECT) to (text CLOG-OBJ)"
  `(defmethod (setf ,accessor) :after (new-value (obj (eql ,object)))
     (setf (text ,clog-obj) new-value)))

