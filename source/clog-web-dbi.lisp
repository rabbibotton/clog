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

(defun load-content (sql-connection table key-value &key
						      (key-col "key")
						      where
						      order-by)
  "Returns list of records found in TABLE where KEY-COL = KEY-VALUE and
optional WHERE and ORDER-BY sql."
  (let ((contents (dbi:fetch-all
		   (dbi:execute
		    (dbi:prepare
		     sql-connection
		     (format nil "select * from ~A where ~A=? ~A ~A"
			     table key-col
			     (if where
				 (format nil "and ~A" where)
				 "")
			     (if order-by
				 (format nil "order by ~A" order-by)
				 "")))
		    (list key-value)))))
    contents))

;;;;;;;;;;;;;;;;;;;;;;
;; clog-web-content ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun clog-web-content (sql-connection
			 &key
			   (page              "main")
			   (table             "content")
			   (base-url          "/content")
			   (follow-url-page   t)
			   comment-table
			   on-new
			   on-edit
			   on-delete
			   (can-comment       :content-comment)
			   (can-show-comments :content-show-comments)
			   (can-edit          :content-edit))
  "Create content for CLOG-WEB:CREATE-WEB-PAGE based on dbi TABLE
value where key=PAGE or if FOLLOW-URL-PAGE is true PAGE is default
page if no second on path otherwise page is the second on path (first
must be base-url). If comment-table is nil no comments are shown. User
must authorize on CAN-COMMENT, CAN-SHOW-COMMENTS and if CAN-EDIT."
  (lambda (obj)
    (let* ((body    (connection-body obj))
	   (prof    (profile (get-web-site body)))
	   (roles   (roles (get-web-site body)))
	   (url     (base-url-split base-url (path-name (location body)))))
      (when follow-url-page
	(when (second url)
	  (setf page (second url))))
      (let ((content (getf (car (load-content
				 sql-connection table page))
			   :|value|)))
	(when content
	  (create-div body :content content)))
      (when (and (clog-auth:is-authorized-p roles can-show-comments)
		 comment-table)
	(let ((comments (load-content sql-connection comment-table page
				      :key-col  "parent"
				      :order-by "createdate desc")))
	  (dolist (comment comments)
	    (create-div obj :content (getf comment :|value|)))))
      (create-div body :content (format nil "<br>prof = ~A<br>url = '~A'<br>roles = ~A"
					prof (second url) roles)))))
