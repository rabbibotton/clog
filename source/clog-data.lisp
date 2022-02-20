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
  "Returns a list, one value for each slot name in SLOT-NAME-LIST. If
a slot contains a CLOG-ELEMENT then TEXT-VALUE is used to retrieve the
value. Slot names may be symbols, keywords or text (and will be
upcased before looking up symbol). All slot-names must be bound."
  (let ((result))
    (dolist (slot (reverse slot-name-list))
      (when (keywordp slot)
	(setf slot (format nil "~A" slot)))
      (unless (symbolp slot)
	(setf slot (find-symbol (string-upcase slot))))
      (if (and (slot-boundp obj slot)
	       (typep (slot-value obj slot) 'clog:clog-element))
	  (push (text-value (slot-value obj slot)) result)
	  (push (slot-value obj slot) result)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;
;;  data-write-plist ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun data-write-plist (obj slot-name-list &key (keys-as-keywords t))
  "Returns a plist, one member for each slot name in SLOT-NAME-LIST,
the key is the slot name. If a slot contains a CLOG-ELEMENT then
TEXT-VALUE is used to retrieve the value otherwise it is the
slot-value. Slot names may be symbols, keywords, or text (and will be
upcased before looking up symbol). All slot-names must be bound. If
:KEYS-AS-KEYWORDS t (default) then the keys will be symbols in the
keyword package."
  (let ((result))
    (dolist (slot (reverse slot-name-list))
      (when (keywordp slot)
	(setf slot (format nil "~A" slot)))
      (unless (symbolp slot)
	(setf slot (find-symbol (string-upcase slot))))
      (if (and (slot-boundp obj slot)
	       (typep (slot-value obj slot) 'clog:clog-element))
	  (push (text-value (slot-value obj slot)) result)
	  (push (slot-value obj slot) result))
      (if keys-as-keywords
	  (push (find-symbol (format nil "~A" slot) 'keyword) result)
	  (push slot result)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - simple sql writers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; sql-list ;;
;;;;;;;;;;;;;;

(defun sql-list (field-list &key quote-all)
  "Given list of fields returns a string for use in a SQL select and
insert field lists. Use a cons to rename fields for selects if
desired. Symbols are stringified first. If :QUOTE-ALL t then all
fields are in quotes."
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

;;;;;;;;;;;;;;;;;;;;
;; sql-value-list ;;
;;;;;;;;;;;;;;;;;;;;

(defun sql-value-list (value-list)
  "Given list of values returns a string for use in a SQL insert value
list. If a value is a string it is quoted with single quotes
(and single quotes qutoed by doubling) unless is the single
character '?'."
  (let ((result))
    (dolist (value (reverse value-list))
      (setf value (format nil "~A~A"
			  (if (and (stringp value)
				   (not (equal value "?")))
			      (format nil "'~A'"
				      (ppcre:regex-replace-all "'" value "''"))
			      (format nil "~A" value))
			  (if result ", " "")))
    (push value result))
  (format nil "~{~A~}" result)))

;;;;;;;;;;;;;;;;;;;;;
;; sql-update-list ;;
;;;;;;;;;;;;;;;;;;;;;

(defun sql-update-list (plist)
  "Given plist of field names and values returns a string for use in a
SQL update. If a value is a string it is quoted with single quotes
(and single quotes qutoed by doubling) unless is the single
character '?'."
  (let ((result))
    (loop for (key value) on plist by #'cddr while value 
	  do
	     (push (format nil "~A = ~A~A"
			   key
			   (if (and (stringp value)
				    (not (equal value "?")))
			       (format nil "'~A'"
				       (ppcre:regex-replace-all "'" value "''"))
			       (format nil "~A" value))
			   (if result ", " ""))
		   result))
    (format nil "~{~A~}" result)))

