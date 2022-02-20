;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-data.lisp                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;; Various functions for binding data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - data load and write from objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;; data-load-plist ;;
;;;;;;;;;;;;;;;;;;;;;

(defun data-load-plist (obj plist &key (upcase-key t))
  "Load a PLIST in to OBJ where key of plist is the name of slot on
OBJ and the value is the data to load.  If slot is a CLOG-ELEMENT
TEXT-VALUE is set, if not the slot is set to the value. If key is not
the name of a slot it is ignored.  The key is coverted to a string and
upper cased before attempting to match it to a slot if :UPCASE-KEY t
(default)."
  (loop for (key value) on plist by #'cddr while value 
	do
	   (let* ((slot-str  (format nil "~A" key))
		  (slot-name (if upcase-key
				 (string-upcase slot-str)
				 slot-str)))
	     (when (find-symbol slot-name)
	       (let ((slot-sym (intern slot-name)))	     
		 (when (slot-exists-p obj slot-sym)
		   (if (and (slot-boundp obj slot-sym)
			    (typep (slot-value obj slot-sym) 'clog:clog-element))
		       (setf (text-value (slot-value obj slot-sym)) value)
		       (setf (slot-value obj slot-sym) value))))))))

;;;;;;;;;;;;;;;;;;;;;;
;;  data-write-list ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun data-write-list (obj slot-name-list)
  "Returns a list, one member for each slot name in SLOT-NAME-LIST. If
a slot contains a CLOG-ELEMENT then TEXT-VALUE is used to retrieve the
value. Slot names may be symbols or text (and will be upcased before
looking up symbol). All slot-names must be bound."
  (let ((result))
    (dolist (slot (reverse slot-name-list))
      (unless (symbolp slot)
	(setf slot (find-symbol (string-upcase slot))))
      (if (and (slot-boundp obj slot)
	       (typep (slot-value obj slot) 'clog:clog-element))
	  (push (text-value (slot-value obj slot)) result)
	  (push (slot-value obj slot) result)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - simple sql writers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;; select-fields ;;
;;;;;;;;;;;;;;;;;;;

(defun select-fields (field-list &key quote-all)
  "Given list of fields return a string to use in a SQL select. Use a cons to
rename field if desired. Symbols are stringified first. If :QUOTE-ALL t then
all fields are in quotes for use on case sensitive SQL dbms.
 Eg. ((\"a\" \"index\") 'b :|c|) would return -> a as 'index', B, c"
  (let ((result))
    (dolist (field (reverse field-list))
      (if (consp field)
	  (setf field (format nil "~A as '~A'~A"
			      (if quote-all
				  (format nil "'~A'" (first field))
				  (format nil "~A" (first field)))
			      (second field)
			      (if result ", " "")))
	  (setf field (format nil "~A~A"
			      (if quote-all
				  (format nil "'~A'" field)
				  (format nil "~A" field))
			      (if result ", " ""))))
      (push field result))
    (format nil "~{~A~}" result)))

