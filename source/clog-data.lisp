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
;; Implementation - data binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun data-load-plist (obj plist &key (upcase-key t))
  "Load a PLIST in to OBJ where key of plist is the name of slot on
OBJ and the value is the data to load.  If slot is a clog-element
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
  
  
