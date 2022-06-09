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

(defun data-load-plist (obj plist &key (row-id-name nil) (upcase-key t))
  "Load a PLIST in to OBJ where key of plist is the name of slot on
OBJ and the value is the data to load.  If slot contains a CLOG-ELEMENT
TEXT-VALUE is set, if not the slot is set to the value. If key is not
the name of a slot it is ignored.  The key is coverted to a string and
upper cased before attempting to match it to a slot if :UPCASE-KEY t
(default). If :ROW-ID-NAME is set returns that fields value."
  (let ((result))
    (loop for (key value) on plist by #'cddr while value
          do
             (when (consp key)
               (setf key (second key)))
             (let* ((slot-str  (format nil "~A" key))
                    (slot-name (if upcase-key
                                   (string-upcase slot-str)
                                   slot-str))
                    (slot-sym  (find slot-name (closer-mop:compute-slots (class-of obj))
                                     :key #'closer-mop:slot-definition-name
                                     :test #'string=)))
               (when (equalp row-id-name slot-name)
                 (setf result value))
               (when slot-sym
                 (setf slot-sym (closer-mop:slot-definition-name slot-sym))
                 (if (and (slot-boundp obj slot-sym)
                          (typep (slot-value obj slot-sym) 'clog:clog-element))
                     (setf (text-value (slot-value obj slot-sym)) value)
                     (setf (slot-value obj slot-sym) value)))))
    result))

;;;;;;;;;;;;;;;;;;;;;;
;;  data-write-list ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun data-write-list (obj slot-name-list &key (upcase-key t))
  "Returns a list, one value for each slot name in SLOT-NAME-LIST. If
a slot contains a CLOG-ELEMENT then TEXT-VALUE is used to retrieve the
value. Slot names may be symbols, keywords or text (and will be
upcased before looking up symbol if :UPCASE-KEY t). All slot-names
must be bound."
  (let ((result))
    (dolist (slot (reverse slot-name-list))
      (when (consp slot)
          (setf slot (second slot)))
      (when (keywordp slot)
        (setf slot (format nil "~A" slot)))
      (unless (symbolp slot)
        (when upcase-key
          (setf slot (string-upcase slot))))
      (setf slot (closer-mop:slot-definition-name
                  (find slot (closer-mop:compute-slots (class-of obj))
                        :key #'closer-mop:slot-definition-name
                        :test #'string=)))
      (if (and (slot-boundp obj slot)
               (typep (slot-value obj slot) 'clog:clog-element))
          (push (text-value (slot-value obj slot)) result)
          (push (slot-value obj slot) result)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;
;;  data-write-plist ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun data-write-plist (obj slot-name-list &key (upcase-key t)
                                              (keys-as-keywords t))
  "Returns a plist, one member for each slot name in SLOT-NAME-LIST,
the key is the slot name. If a slot contains a CLOG-ELEMENT then
TEXT-VALUE is used to retrieve the value otherwise it is the
slot-value. Slot names may be symbols, keywords, or text (and will be
upcased before looking up symbol if :UPCASE-KEY t). If a slot-name is
a cons, the first will be used as the key in the plist and the second
will be the actual slot-name. All slot-names must be bound. If
slot-name does not exist it is left out of returned plist. If
:KEYS-AS-KEYWORDS t (default) then the keys will be symbols in the
keyword package."
  (let (result
        pname)
    (dolist (slot (reverse slot-name-list))
      (cond ((consp slot)
             (setf pname (first slot))
             (setf slot (second slot)))
            (t
             (setf pname slot)))
      (when (keywordp slot)
        (setf slot (format nil "~A" slot)))
      (unless (symbolp slot)
        (when upcase-key
          (setf pname (string-upcase pname))
          (setf slot (string-upcase slot))))
      (setf slot (find slot (closer-mop:compute-slots (class-of obj))
                        :key #'closer-mop:slot-definition-name
                        :test #'string=))
      (when slot
        (setf slot (closer-mop:slot-definition-name slot))
        (if (and (slot-boundp obj slot)
                 (typep (slot-value obj slot) 'clog:clog-element))
            (push (text-value (slot-value obj slot)) result)
            (push (slot-value obj slot) result))
        (if keys-as-keywords
            (push (intern (format nil "~A" pname) 'keyword) result)
            (push pname result))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - simple sql writers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *mysql-timestamp* "unix_timestamp()"
  "Function used by mysql to retrieve the time from the epoch")
(defparameter *sqlite-timestamp* "strftime('%s')"
  "Function used by sqlite to retrieve the time from the epoch")
(defparameter *postgresql-timestamp* "extract(epoch from now())"
  "Function used by postgresql to retrieve the time from the epoch")

;;;;;;;;;;;;;;;;;;;;
;; sql-field-list ;;
;;;;;;;;;;;;;;;;;;;;

(defun sql-field-list (field-list &key quote-all for-insert)
  "Given list of fields returns a string for use in a SQL select and
insert field lists. Use a cons (realname asname) to rename fields for
selects, if :FOR-INSERT t then the realname is used. Symbols are
stringified first. If :QUOTE-ALL t then all fields are in quotes."
  (let ((result))
    (dolist (field (reverse field-list))
      (if (and for-insert
               (consp field))
          (setf field (car field)))
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

;;;;;;;;;;;;;;;
;; sql-quote ;;
;;;;;;;;;;;;;;;

(defun sql-quote (value)
  "Returns value single quoted if string (single quote quoted by doubling)
unless is the single character '?'. If value is a list the car is returned
unquoted"
  (cond ((and (stringp value)
              (not (equal value "?")))
         (format nil "'~A'"
                 (ppcre:regex-replace-all "'" value "''")))
        ((consp value)
         (car value))
        (t
         value)))

;;;;;;;;;;;;;;;;;;;;
;; sql-value-list ;;
;;;;;;;;;;;;;;;;;;;;

(defun sql-value-list (value-list)
  "Given list of values each passed to SQL-QUOTE returns a string for
use in a SQL insert value list."
  (let ((result))
    (dolist (value (reverse value-list))
      (setf value (format nil "~A~A"
                          (sql-quote value)
                          (if result ", " "")))
    (push value result))
  (format nil "~{~A~}" result)))

;;;;;;;;;;;;;;;;;;;;;
;; sql-update-list ;;
;;;;;;;;;;;;;;;;;;;;;

(defun sql-update-list (plist)
  "Given plist of field names and values each passed to SQL-QUOTE and
returns a string for use in a SQL update. if the 'key' is a cons the
first 'key' used."
  (let ((result))
    (loop for (key value) on plist by #'cddr while value
          do
             (push (format nil "~A = ~A~A"
                           (if (consp key)
                               (car key)
                               key)
                           (sql-quote value)
                           (if result ", " ""))
                   result))
    (format nil "~{~A~}" result)))

;;;;;;;;;;;;;;;;
;; sql-select ;;
;;;;;;;;;;;;;;;;

(defun sql-select (table field-list &key where order-by limit)
  "Build basic sql select statement"
  (format nil "select ~A from ~A~A~A~A"
          (if (consp field-list)
              (sql-field-list field-list)
              field-list)
          (if (consp table)
              (sql-field-list table)
              table)
          (if (and where (not (equal where "")))
              (format nil " where ~A" where)
              "")
          (if (and order-by (not (equal order-by "")))
              (format nil " order by ~A" order-by)
              "")
          (if (and limit (not (equal limit "")))
              (format nil " limit ~A" limit)
              "")))

;;;;;;;;;;;;;;;;
;; sql-insert ;;
;;;;;;;;;;;;;;;;

(defun sql-insert (table field-list value-list)
  "Build basic sql insert statement"
  (format nil "insert into ~A (~A) values (~A)"
          table
          (sql-field-list field-list :for-insert t)
          (sql-value-list value-list)))

;;;;;;;;;;;;;;;;;
;; sql-insert* ;;
;;;;;;;;;;;;;;;;;

(defun sql-insert* (table plist)
  "Build basic sql insert statement using a plist"
  (loop for (key value) on plist by #'cddr while value
        collect key into fields
        collect value into values
        finally (return (sql-insert table fields values))))

;;;;;;;;;;;;;;;;
;; sql-update ;;
;;;;;;;;;;;;;;;;

(defun sql-update (table plist where)
  "Build basic sql update statement"
  (format nil "update ~A set ~A where ~A"
          table
          (sql-update-list plist)
          where))
