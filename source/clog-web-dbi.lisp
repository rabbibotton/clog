;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-web-dbi.lisp                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Database components for use with clog-web-website

(mgl-pax:define-package :clog-web-dbi
  (:documentation "CLOG-WEB-DBI - dbi based website helpers")
  (:use #:cl #:parse-float #:clog #:clog-web #:clog-auth #:mgl-pax))

(cl:in-package :clog-web-dbi)

(defsection @clog-web-dbi (:title "CLOG Web DBI")
  "Authentication"
  (login              function)
  (logout             function)
  (get-profile        function)
  (sign-up            function)
  (make-token         function)
  (load-content       function)
  (create-base-tables function))

;;;;;;;;;;;;;;;;;
;; get-profile ;;
;;;;;;;;;;;;;;;;;

(defun get-profile (obj sql-connection)
  "Retrieve profile based on current authentication token. If there is
no token or fails to match as user returns nil"
  (let* ((body  (connection-body obj))
	 (token (clog-auth:get-authentication-token body)))
    (when token
      (let ((contents (dbi:fetch-all
		       (dbi:execute
			(dbi:prepare
			 sql-connection
			 "select * from users where token=?")
			(list token)))))
	(when contents
	  (car contents))))))

;;;;;;;;;;;
;; login ;;
;;;;;;;;;;;

(defun login (body sql-connection username password)
  "Login and set current authentication token, it does not remove token
if one is present and login fails."
  (check-type body clog-body)
  (let ((contents (dbi:fetch-all
		   (dbi:execute
		    (dbi:prepare
		     sql-connection
		     "select token from users where username=? and password=?")
		    (list username password)))))
    (when contents
      (store-authentication-token body (getf (car contents) :|token|)))))

;;;;;;;;;;;;
;; logout ;;
;;;;;;;;;;;;

(defun logout (body)
  "Logout and remove current authenitcation token"
  (check-type body clog-body)
  (remove-authentication-token body))

;;;;;;;;;;;;;
;; sign-up ;;
;;;;;;;;;;;;;

(defun sign-up (body sql-connection &key (title "Sign Up")
				      (next-step "/login"))
  (check-type body clog-body)
  (clog-web-form
   body title
   `(("Username" "username")
     ("Password" "password" :password)
     ("Retype Password" "repass" :password))
   (lambda (result)
     (cond ((not
	     (equal (form-result result "password")
		    (form-result result "repass")))
	    (clog-web-alert body "Mismatch"
			    "The passwords do match."
			    :time-out 3
			    :place-top t))
	   ((< (length (form-result result "password")) 4)
	    (clog-web-alert body "Missize"
			    "The passwords must at least 4 characters."
			    :time-out 3
			    :place-top t))
	   ((< (length (form-result result "username")) 4)
	    (clog-web-alert body "Missize"
			    "The username must be at least 4 characters."
			    :time-out 3
			    :place-top t))
	   (t
	    (let ((contents (dbi:fetch-all
			     (dbi:execute
			      (dbi:prepare
			       sql-connection
			       "select username from users where username=?")
			      (list (form-result result "username"))))))
	      (cond (contents
		     (clog-web-alert body "Exists"
				     "The username is not available."
				     :time-out 3
				     :place-top t))
		    (t
		     (dbi:do-sql
		       sql-connection
		       (sql-insert*
			"users"
			`(:username ,(form-result result "username")
			  :password ,(form-result result "password")
			  :token    ,(make-token))))
		     (url-replace (location body) next-step)))))))))

;;;;;;;;;;;;;;;;
;; make-token ;;
;;;;;;;;;;;;;;;;

(defun make-token ()
  (get-universal-time))

;;;;;;;;;;;;;;;;;;;;;;;
;; create-base-table ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun create-base-tables (sql-connection)
  (dbi:do-sql
    sql-connection
    "create table config (key varchar, value varchar)")
  (dbi:do-sql
    sql-connection
    (format nil "create table content (key varchar, value varchar, parent varchar, title varchar, username varchar, createdate date)"))
  (dbi:do-sql
    sql-connection
    "create table tags (key varchar, value varchar, category varchar)")
  (dbi:do-sql
    sql-connection
    "create table users (username varchar, password varchar, token varchar)")
  (dbi:do-sql
    sql-connection
    (sql-insert* "content" '(:key        "main"
			     :value      "<h3>Welcome to CLOG</h3>"
			     :createdate ("date()"))))
  (dbi:do-sql
    sql-connection
    (sql-insert* "users" `(:username "admin"
			   :password "admin"
			   :token    ,(make-token)))))

;;;;;;;;;;;;;;;;;;
;; load-content ;;
;;;;;;;;;;;;;;;;;;

(defun load-content (sql-connection table key-value &key (key-col "key"))
  "Returns list of records found in TABLE where KEY-COL = KEY-VALUE"
  (let ((contents (dbi:fetch-all
		   (dbi:execute
		    (dbi:prepare
		     sql-connection
		     (format nil "select * from ~A where ~A=?"
			     table key-col))
		    (list key-value)))))
    contents))
