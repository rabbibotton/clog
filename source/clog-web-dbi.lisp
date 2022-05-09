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
    (format nil "create table content (key varchar, value varchar,~
                                       parent varchar, title varchar,~
                                       username varchar, createdate date)"))
  (dbi:do-sql
    sql-connection
    "create table tags (key varchar, value varchar, category varchar)")
  (dbi:do-sql
    sql-connection
    "create table users (username varchar, password varchar, token varchar)")
  (dbi:do-sql
    sql-connection
    (sql-insert* "content" '(:key        "main"
			     :title      "Welcome to CLOG"
			     :value      "Sample data"
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
			   on-content
			   on-comment
			   on-new
			   on-edit
			   on-delete
			   (can-comment       :content-comment)
			   (can-show-comments :content-show-comments)
			   (can-edit          :content-edit))
  "Create content for CLOG-WEB:CREATE-WEB-PAGE based on dbi TABLE
value where key=PAGE or if FOLLOW-URL-PAGE is true PAGE is default
page if no second on path otherwise page is the second on path (first
must be base-url). ON-CONTENT, ON-COMMENT are called with (obj value)
before display of value the return value is used. ON-NEW, ON-EDIT are
called with (obj value) to allow filter of value before storage, if
nil is return aborted. ON-DELETE called with (obj page comment-id) if
returns nil aborted. If comment-table is nil no comments are
shown. User must authorize on action set by CAN-COMMENT,
CAN-SHOW-COMMENTS and if CAN-EDIT unless they are set to nil. The URL
scheme as as follows:

to add content    - baseurl/add
or add content    - baseurl/page/add
to delete content - baseurl/page/delete
to delete comment - baseurl/page/comment/comment-id/delete
"
  (lambda (obj)
    (let* ((body    (connection-body obj))
	   (theme   (theme (get-web-site body)))
	   (prof    (profile (get-web-site body)))
	   (roles   (roles (get-web-site body)))
	   (url     (base-url-split base-url (path-name (location body)))))
      ;; Set page to show content
      (when follow-url-page
	(when (second url)
	  (setf page (second url))))
      ;; Perform commands on content and comments
      (cond ((equalp (third url) ;; delete content
		     "delete")
	     (when (clog-auth:is-authorized-p roles can-edit)
	       (if on-delete
		   (setf on-delete (setf on-delete (funcall on-delete obj page nil)))
		   (setf on-delete t))
	       (when on-delete
		 (dbi:do-sql
		   sql-connection
		   (format nil "delete from ~A where key=?" table)
		   (list page)))))
	    ((and (and (third url) ;; delete comment
		       "comment")
		  (equalp (fifth url)
			  "delete"))
	     (when (clog-auth:is-authorized-p roles can-edit)
	       (if on-delete
		   (setf on-delete (funcall on-delete obj page (fourth url)))
		   (setf on-delete t))
	       (when on-delete
		 (dbi:do-sql
		   sql-connection
		   (format nil "delete from ~A where key=?" comment-table)
		   (list (fourth url)))))))
      (let ((content (car (load-content
			   sql-connection table page))))
	(when content
	  (when on-content
	    (setf content (funcall on-content obj content)))
	  (funcall theme obj :content-body
		   (list :content content
			 :base-url (format nil "~A/~A" base-url page)
			 :save-edit (lambda (content)
				      (when on-edit
					(setf content (funcall on-edit content)))
				      (dbi:do-sql
					sql-connection
					(sql-update table
						    content
						    "key=?")
					(list page)))
			 :can-edit (clog-auth:is-authorized-p roles can-edit)
			 :new-comment (lambda (content)
					(push `("unixepoch()") content)
					(push :createdate content)
					(push `("unixepoch()") content)
					(push :key content)
					(push page content)
					(push :parent content)
					(push (getf prof :|username|) content)
					(push :username content)
					(when on-new
					  (setf content (funcall on-new content)))
					(dbi:do-sql
					  sql-connection
					  (sql-insert* comment-table content)))
			 :can-comment (clog-auth:is-authorized-p
				       roles can-comment)))))
      (when (and (clog-auth:is-authorized-p roles can-show-comments)
		 comment-table)
	(let ((comments (load-content sql-connection comment-table page
				      :key-col  "parent"
				      :order-by "createdate desc")))
	  (dolist (comment comments)
	    (when on-comment
	      (setf comment (funcall on-comment obj comment)))
	    (funcall theme obj :content-comment
		     (list :content comment
			   :base-url (format nil "~A/~A/comment/~A"
					     base-url
					     page
					     (getf comment :|key|))
			   :save-edit (lambda (content)
					(when on-edit
					  (setf content (funcall on-edit content)))
					(dbi:do-sql
					  sql-connection
					  (sql-update comment-table
						      content
						      "key=?")
					  (list (getf comment :|key|))))
			   :can-edit (and (getf prof :|username|)
					  (equalp (getf comment :|username|)
						  (getf prof    :|username|)))
			   :can-comment (clog-auth:is-authorized-p
					 roles can-comment)))))))))
