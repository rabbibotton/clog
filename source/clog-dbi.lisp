;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-dbi.lisp                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;; CLOG Elements for Interfacing with databases. The current implemnitation
;;; uses cl-dbi. The purpose of creating "visual" elements is to facilitate
;;; their use in CLOG Builder

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - CLOG-Database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-database (clog-element)
  ((connection
    :accessor database-connection
    :initform nil
    :documentation "Connection handle to database"))
  (:documentation "Connection to database."))

;;;;;;;;;;;;;;;;;;;;;
;; create-database ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-database (clog-obj
			     &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Database element, for use in
CLOG-Builder. If not using builder use to connect:
    (dbi:connect (database-connection clog-obj) ...)"))

(defmethod create-database ((obj clog-obj)
			    &key (hidden nil)
			      (class nil)
			      (html-id nil) (auto-place t))
  (let ((new-obj (change-class (create-div obj :content ""
					       :hidden hidden
					       :class class
					       :html-id html-id
					       :auto-place auto-place)
			       'clog-database)))
    new-obj))
    
;;;;;;;;;;;;;;;;;;;;;;;;;
;; database-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric database-connection (clog-obj)
  (:documentation "Accessor to the database handle"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - CLOG-One-Row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-one-row (clog-element)
  ((clog-database
    :accessor clog-database
    :initform nil
    :documentation "Database control table connected to.")
   (table
    :accessor table-name
    :initform nil
    :documentation "Table name")
   (where-clause
    :accessor where-clause
    :initform nil
    :documentation "Where clause")
   (id
    :accessor row-id-name
    :initform nil
    :documentation "Column used to indicate id of row")
   (rowid
    :accessor rowid
    :initform nil
    :documentation "Current rowid")
   (columns
    :accessor table-columns
    :initform nil
    :documentation "Columns of table to be retrieved"))
  (:documentation "Manipulate one row of a table."))

;;;;;;;;;;;;;;;;;;;;
;; create-one-row ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric create-one-row (clog-obj &key clog-database
				       hidden class html-id auto-place)
  (:documentation "Create a new CLOG-One-Row element. A CLOG Database
must be a parent to CLOG-One-Row."))

(defmethod create-one-row ((obj clog-obj) &key (clog-database nil)
					    (hidden nil)
					    (class nil)
					    (html-id nil) (auto-place t))
  (let ((new-obj (change-class (create-div obj :content ""
					       :hidden hidden
					       :class class
					       :html-id html-id
					       :auto-place auto-place)
			       'clog-one-row)))
    (setf (clog-database new-obj) clog-database)
    new-obj))

(defgeneric get-row (clog-obj panel)
  (:documentation "Get a row from a database table based on
CLOG-OBJECT's table-name using where-clause and table-columns.
row-id-name is required. All PANEL items or custom rows
on panel will be set use DATA-LOAD-PLIST."))
(defmethod get-row ((obj clog-obj) panel)
  (setf (rowid obj) (data-load-plist panel
				     (dbi:fetch
				      (dbi:execute
				       (dbi:prepare
					(database-connection (clog-database obj))
					(sql-select (table-name obj)
						    (table-columns obj)
						    :where (where-clause obj)))))
				     :row-id-name (row-id-name obj))))

(defgeneric insert-row (clog-obj panel)
  (:documentation "Insert new row in to database table based on
CLOG-OBJECT's table-name and table-columns. DATA-WRITE-PLIST is
used to extract data from PANEL items and custom rows."))
(defmethod insert-row ((obj clog-obj) panel)
  (dbi:do-sql (database-connection (clog-database obj))
    (sql-insert* (table-name obj)
		 (data-write-plist panel (table-columns obj)))))

(defgeneric update-row (clog-obj panel)
  (:documentation "Update row in database table based on
CLOG-OBJECT's table-name using current rowid and table-columns.
row-id-name is required. All PANEL items or custom rows
on panel will be retrieved from PANEL using DATA-WRITE-PLIST."))
(defmethod update-row ((obj clog-obj) panel)
  (dbi:do-sql (database-connection (clog-database obj))
    (sql-update (table-name obj)
		(data-write-plist panel (table-columns obj))
		(format nil "~A=~A" (row-id-name obj) (rowid obj)))))

(defgeneric delete-row (clog-obj)
  (:documentation "Delete a row from a database table based on
current rowid"))
(defmethod delete-row ((obj clog-obj))
  (dbi:do-sql (database-connection (clog-database obj))
    (format nil "delete from ~A where ~A=~A"
	    (table-name obj)
	    (row-id-name obj)
	    (rowid obj))))

(defgeneric clear-row (clog-obj panel)
  (:documentation "Clear current rowid and all fields in PANEL
using DATA-WRITE-PLIST based on table-columns."))
(defmethod clear-row ((obj clog-obj) panel)
  (let ((result))
    (dolist (c (table-columns obj))
      (push "" result)
      (push c result))
    (data-load-plist panel result)
    (setf (rowid obj) nil)))

