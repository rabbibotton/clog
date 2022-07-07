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
    (dbi:connect (database-connection clog-obj) ...) or if a
connection exists assign it to the database-connecton."))

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

(defgeneric clog-database (clog-obj)
  (:documentation "Access to the CLOG-DATABASE"))
(defmethod clog-database ((obj clog-database))
  obj)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; database-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric database-connection (clog-database)
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
   (order-by
    :accessor order-by
    :initform nil
    :documentation "Sort by")
   (limit
    :accessor limit
    :initform nil
    :documentation "Limit number of returned rows")
   (id
    :accessor row-id-name
    :initform nil
    :documentation "Column used to indicate id of row")
   (rowid
    :accessor rowid
    :initform nil
    :documentation "Current rowid")
   (last-fetch
    :accessor last-fetch
    :initform nil
    :documentation "Last fetch plist")
   (last-sql
    :accessor last-sql
    :initform nil
    :documentation "Last sql executed")
   (columns
    :accessor table-columns
    :initform nil
    :documentation "Columns of table to be retrieved")
   (queryid
    :accessor queryid
    :initform nil
    :documentation "Current query (private)")
   (slave-to-slot
    :accessor slave-to-slot
    :initform nil
    :documentation "Slot to watch on fetch by master row (private)")
   (slaves
    :accessor slaves
    :initform nil
    :documentation "List of slaves to call get-row")
   (on-fetch
    :accessor on-fetch
    :initform nil
    :documentation "on-fetch event, called after fetch complete. (private)"))
  (:documentation "Manipulate one row of a table at a time on panel."))

;;;;;;;;;;;;;;;;;;;;
;; create-one-row ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric create-one-row (clog-obj &key clog-database
                                       hidden class html-id auto-place)
  (:documentation "Create a new CLOG-One-Row element. If CLOG-OBJ is
of type-of CLOG-DATABASE it is used as database source unless
:CLOG-DATABASE is set."))

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
    (if (and (typep obj 'clog-database) (not clog-database))
        (setf (clog-database new-obj) obj)
        (setf (clog-database new-obj) clog-database))
    new-obj))

(defgeneric query-row (clog-one-row panel sql)
  (:documentation "Ignore query related prperties and instead execute
SQL. row-id-name is required for updates. All PANEL items or custom
slots on panel will be set using DATA-LOAD-PLIST."))
(defmethod query-row ((obj clog-one-row) panel sql)
  (setf (last-sql obj) sql)
  (setf (queryid obj) (dbi:execute
                       (dbi:prepare
                        (database-connection (clog-database obj))
                        sql)))
  (next-row obj panel))

(defgeneric get-row (clog-one-row panel)
  (:documentation "Get first row from a database table based on
CLOG-ONE-ROW's table-name using where-clause and table-columns.
row-id-name is required. All PANEL items or custom slots on panel will
be set using DATA-LOAD-PLIST."))
(defmethod get-row ((obj clog-one-row) panel)
  (let ((where (where-clause obj)))
    (when (slave-to-slot obj)
      (let ((field (slave-to-slot obj))
            (data  (car (data-write-list panel (list (slave-to-slot obj))))))
        (when (consp (slave-to-slot obj))
          (setf field (car field)))
        (setf where (format nil "~A='~A'~A"
                                field
                                data
                                (if (equal where "")
                                    ""
                                    (format nil " and ~A" where))))))
    (setf (last-sql obj) (sql-select (table-name obj)
                                      (table-columns obj)
                                      :where where
                                      :order-by (order-by obj)
                                      :limit (limit obj)))
    (setf (queryid obj) (dbi:execute
                         (dbi:prepare
                          (database-connection (clog-database obj))
                          (last-sql obj)))))
    (next-row obj panel))

(defgeneric next-row (clog-one-row panel)
  (:documentation "Get next row from a database table based on query
made for get-row. All PANEL items or custom slots on panel will be set
using DATA-LOAD-PLIST."))
(defmethod next-row ((obj clog-one-row) panel)
  (dolist (slave (slaves obj))
    (clear-row slave panel))
  (setf (last-fetch obj) (dbi:fetch (queryid obj)))
  (when (on-fetch obj)
    (funcall (on-fetch obj) obj))
  (setf (rowid obj) (data-load-plist panel
                                     (last-fetch obj)
                                     :row-id-name (row-id-name obj)))
  (if (rowid obj)
      (dolist (slave (slaves obj))
        (get-row slave panel))
      (unless (slave-to-slot obj)
        (clear-row obj panel)))
  (rowid obj))

(defgeneric insert-row (clog-one-row panel)
  (:documentation "Insert new row in to database table based on
CLOG-ONE-ROW's table-name and table-columns. DATA-WRITE-PLIST is
used to extract data from PANEL items and custom slots."))
(defmethod insert-row ((obj clog-one-row) panel)
  (dbi:do-sql (database-connection (clog-database obj))
    (sql-insert* (table-name obj)
                 (data-write-plist panel (table-columns obj)))))

(defgeneric update-row (clog-one-row panel)
  (:documentation "Update row in database table based on
CLOG-ONE-ROW's table-name using current rowid and table-columns.
row-id-name is required. All PANEL items or custom rows
on panel will be retrieved from PANEL using DATA-WRITE-PLIST."))
(defmethod update-row ((obj clog-one-row) panel)
  (dbi:do-sql (database-connection (clog-database obj))
    (sql-update (table-name obj)
                (data-write-plist panel (table-columns obj))
                (format nil "~A=~A" (row-id-name obj) (rowid obj)))))

(defgeneric delete-row (clog-one-row panel)
  (:documentation "Delete a row from a database table based on
current rowid and then call CLEAR-ROW"))
(defmethod delete-row ((obj clog-one-row) panel)
  (dbi:do-sql (database-connection (clog-database obj))
    (format nil "delete from ~A where ~A=~A"
            (table-name obj)
            (row-id-name obj)
            (rowid obj)))
  (clear-row obj panel))

(defgeneric clear-row (clog-one-row panel)
  (:documentation "Clear current rowid and all fields in PANEL
using DATA-WRITE-PLIST based on table-columns."))
(defmethod clear-row ((obj clog-one-row) panel)
  (let ((result))
    (dolist (c (table-columns obj))
      (push "" result)
      (push c result))
    (data-load-plist panel result)
    (setf (last-fetch obj) nil)
    (setf (rowid obj) nil)))

(defgeneric set-master-one-row (clog-one-row master-one-row slot-name)
  (:documentation "Set CLOG-ONE-ROW to get-row setting a while-clause
 to follow slot-name of panel when MASTER-ONE-ROW calls next-row."))
(defmethod set-master-one-row ((obj clog-one-row) master-one-row slot-name)
  (when (typep master-one-row 'clog-one-row)
    (push obj (slaves master-one-row)))
  (setf (slave-to-slot obj) slot-name))

(defgeneric set-on-fetch (clog-one-row on-fetch-handler)
  (:documentation "Set the ON-FETCH-HANDLER for CLOG-ONE-ROW. If ON-FETCH-HANDLER
is nil unbind the event. The on-fetch event is called after the row was fetched
and stored in (last-fetch clog-one-row) or nil if no row was returned, and before
data-load-plist is called that will use the value of (last-fetch clog-one-row).
Calculated fields, transformations to field values, etc. can be done in on-fetch as
new-row will block until on-fetch returns."))
(defmethod set-on-fetch ((obj clog-one-row) on-fetch-handler)
  (setf (on-fetch obj) on-fetch-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-lookup (clog-one-row clog-select)
  ((value-field
    :accessor value-field
    :initform nil
    :documentation "Field used to for value of lookup option.
                    Case sensitive keyword")
   (option-field
    :accessor option-field
    :initform nil
    :documentation "Filed used to display for value of lookup option.
                    Case sensitive keyword"))
  (:documentation "CLOG Table Lookup Object"));

;;;;;;;;;;;;;;;;;;;
;; create-lookup ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-lookup (clog-obj &key name multiple label class html-id)
  (:documentation "Create a new clog-lookup as child of CLOG-OBJ."))

(defmethod create-lookup ((obj clog-obj)
                          &key (clog-database nil)
                            (name nil)
                            (multiple nil)
                            (label nil)
                            (class nil)
                            (html-id nil))
  (let ((element (create-child
                  obj (format nil "<select~A~A~A/>"
                              (if multiple
                                  " multiple"
                                  "")
                              (if name
                                  (format nil " name='~A'" name)
                                  "")
                              (if class
                                  (format nil " class='~A'"
                                          (escape-to-single-quote-in-tag class))
                                  ""))
                 :clog-type 'clog-lookup :html-id html-id :auto-place t)))
    (when label
      (label-for label element))
    (if (and (typep obj 'clog-database) (not clog-database))
        (setf (clog-database element) obj)
        (setf (clog-database element) clog-database))
    element))

(defmethod next-row ((obj clog-lookup) panel)
  "In clog-lookup objects, next-row adds options to lookup's
select tag for every row returned. The option value is set to
the VALUE-FIELD property and the OPTION-FIELD property is the
the displayed option."
  (dolist (slave (slaves obj))
    (clear-row slave panel))
  ;; loop through fetches
  (let ((select-value (text-value obj)))
    (setf (rowid obj) nil)
    (setf (inner-html obj) "")
    (loop
      (let ((selected nil)
            (row      (dbi:fetch (queryid obj))))
        (unless row
          (return))
        (when (on-fetch obj)
          (funcall (on-fetch obj) obj))
        (when (equal select-value (getf row (value-field obj)))
          (setf selected t)
          (setf (rowid obj) (data-load-plist panel
                                             (last-fetch obj)
                                             :row-id-name (row-id-name obj))))
        (add-select-option obj
                           (getf row (value-field obj))
                           (getf row (option-field obj))
                           :selected selected))))
  (dolist (slave (slaves obj))
    (get-row slave panel))
  (rowid obj))

(defmethod clear-row ((obj clog-lookup) panel)
  (setf (inner-html obj) "")
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-db-table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-db-table (clog-one-row clog-table)
  ((on-header
    :accessor on-header
    :initform nil
    :documentation "on-header event, called after get-row and
                    before outputing rows. (private)")
   (on-footer
    :accessor on-footer
    :initform nil
    :documentation "on-footer event, called after get-row and
                    before outputing rows. (private)")
   (on-row
    :accessor on-row
    :initform nil
    :documentation "on-row event. (private)")
   (on-column
    :accessor on-column
    :initform nil
    :documentation "on-column. (private)"))
   (:documentation "CLOG Database Table View Object"))

;;;;;;;;;;;;;;;;;;;;;
;; create-db-table ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-db-table (clog-obj &key clog-database
                                        hidden class html-id auto-place)
  (:documentation "Create a new clog-db-table as child of CLOG-OBJ."))

(defmethod create-db-table ((obj clog-obj)
                         &key (clog-database nil)
                           (hidden nil)
                           (class nil) (html-id nil) (auto-place t))
  (let ((element (create-child obj (format nil "<table~A~A/>"
                                           (if hidden
                                               " style='visibility:hidden;'"
                                               "")
                                           (if class
                                               (format nil " class='~A'"
                                                       (escape-to-single-quote-in-tag class))
                                               ""))
                               :clog-type  'clog-db-table
                               :html-id    html-id
                               :auto-place auto-place)))
    (if (and (typep obj 'clog-database) (not clog-database))
        (setf (clog-database element) obj)
        (setf (clog-database element) clog-database))
    element))

(defmethod next-row ((obj clog-db-table) panel)
  "In clog-db-table objects, next-row adds multiple rows to the table."
  (dolist (slave (slaves obj))
    (clear-row slave panel))
  ;; loop through fetches
  (setf (rowid obj) nil)
  (setf (inner-html obj) "")
  (when (on-header obj)
    (funcall (on-header obj) obj))
  (loop
    (let ((row (dbi:fetch (queryid obj))))
      (unless row
        (return))
      (when (on-fetch obj)
        (funcall (on-fetch obj) obj))
      (let ((tr (create-table-row obj)))
        (when (on-row obj)
          (funcall (on-row obj) obj tr))
        (loop for (key value) on row by #'cddr while value
              do
                 (let ((td (create-table-column obj :content value)))
                   (when (on-column obj)
                     (funcall (on-column obj) obj key td)))))))
  (when (on-footer obj)
    (funcall (on-footer obj) obj))
  (dolist (slave (slaves obj))
    (get-row slave panel))
  (rowid obj))

(defmethod clear-row ((obj clog-db-table) panel)
  (setf (inner-html obj) "")
  (call-next-method))

(defgeneric set-on-header (clog-db-table on-header-handler)
  (:documentation "Set the ON-HEADER-HANDLER for CLOG-DB-TABLE. If ON-HEADER-HANDLER
is nil unbind the event. The on-header event is called before the first row is output
after the table is cleared to all adding a header information to the table."))
(defmethod set-on-header ((obj clog-db-table) on-header-handler)
  (setf (on-header obj) on-header-handler))

(defgeneric set-on-footer (clog-db-table on-footer-handler)
  (:documentation "Set the ON-FOOTER-HANDLER for CLOG-DB-TABLE. If ON-FOOTER-HANDLER
is nil unbind the event. The on-footer event is called after all rows are output
after the table is cleared for adding footer information to the table."))
(defmethod set-on-footer ((obj clog-db-table) on-footer-handler)
  (setf (on-footer obj) on-footer-handler))

(defgeneric set-on-row (clog-db-table on-row-handler)
  (:documentation "Set the ON-ROW-HANDLER for CLOG-DB-TABLE. If ON-ROW-HANDLER
is nil unbind the event. The on-row event is called for each row. The row handler
is passed also the clog-table-row object before the columns are added in second parameter to
handler."))
(defmethod set-on-row ((obj clog-db-table) on-row-handler)
  (setf (on-row obj) on-row-handler))

(defgeneric set-on-column (clog-db-table on-column-handler)
  (:documentation "Set the ON-COLUMN-HANDLER for CLOG-DB-TABLE. If ON-COLUMN-HANDLER
is nil unbind the event. The on-column event is called for each column as added to
the current row being processsed. It is passed also the keyworld symbol name of
the column and the clog-table-column object."))
(defmethod set-on-column ((obj clog-db-table) on-column-handler)
  (setf (on-column obj) on-column-handler))
