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

(defsection @clog-web-dbi (:title "CLOG Web DBI - dbi based website")
  "Authentication"
  (login              function)
  (logout             function)
  (get-profile        function)
  (sign-up            function)
  (make-token         function)
  (create-base-tables function))

;;;;;;;;;;;;;;;;;
;; get-profile ;;
;;;;;;;;;;;;;;;;;

(defun get-profile (obj sql-connection)
  "Retrieve profile based on current authentication token. If there is
no token or fails to match as user returns nil"
  (let ((token (clog-auth:get-authentication-token obj)))
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

(defun login (obj sql-connection username password)
  "Login and set current authentication token, it does not remove token
if one is present and login fails."
  (let ((contents (dbi:fetch-all
		   (dbi:execute
		    (dbi:prepare
		     sql-connection
		     "select token from users where username=? and password=?")
		    (list username password)))))
    (when contents
      (store-authentication-token obj (getf (car contents) :|token|)))))

;;;;;;;;;;;;
;; logout ;;
;;;;;;;;;;;;

(defun logout (obj)
  "Logout and remove current authenitcation token"
  (remove-authentication-token obj))

;;;;;;;;;;;;;
;; sign-up ;;
;;;;;;;;;;;;;

(defun sign-up (obj sql-connection &key (title "Sign Up")
				     (next-step "/login"))
  (clog-web-form
   obj title
   `(("Username" "username")
     ("Password" "password" :password)
     ("Retype Password" "repass" :password))
   (lambda (result)
     (cond ((not
	     (equal (form-result result "password")
		    (form-result result "repass")))
	    (clog-web-alert obj "Mismatch"
			    "The passwords do match."
			    :time-out 3
			    :place-top t))
	   ((< (length (form-result result "password")) 4)
	    (clog-web-alert obj "Missize"
			    "The passwords must at least 4 characters."
			    :time-out 3
			    :place-top t))
	   ((< (length (form-result result "username")) 4)
	    (clog-web-alert obj "Missize"
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
		     (clog-web-alert obj "Exists"
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
		     (url-replace (location obj) next-step)))))))))

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
    "create table users (username varchar, password varchar, token varchar)")
  (dbi:do-sql
    sql-connection
    (sql-insert* "users" `(:username "admin"
			   :password "admin"
			   :token    ,(make-token)))))

