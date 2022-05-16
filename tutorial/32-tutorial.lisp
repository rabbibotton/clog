;; In this tutorial we expand on the last using clog-web-content
;; to instantly create a site with user, authentication, and
;; content management including comments. We also use the option
;; :extended-routing to allow handlers to handle routes on the
;; same path.


(defpackage #:clog-tut-32
  (:use #:cl #:clog #:clog-web #:clog-auth #:clog-web-dbi)
  (:export start-tutorial))

(in-package #:clog-tut-32)

;;
;; Setup website structure, database and CLOG
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *sql-connection* nil)

;; Default user/pass is username: admin and password: admin

;; /content is our root content URL, if you are authorized as an
;; editor or admin you are able to add additional pages by going to
;; the url /content/whatever and then click to add page. If you want
;; it in the menu you would just need to add the url to the
;; menu. There is no need to add handlers for pages under /content as
;; when we initalized CLOG we used the option :extended-routing so
;; that a URL start with /content/ will be sent to the same handler as
;; /content in this case on-main. So our about page has no handler set
;; but functions as we added to out database.

                      ; Menu         Menu Item         URL        Handler     Actions Auth
(defparameter *menu* `(("Features" (("Home"            "/")
				    ("Login"           "/login"   on-login    :login)
				    ("Signup"          "/signup"  on-signup   :signup)
				    ("Change Password" "/pass"    on-new-pass :change-password)
				    ("Content"         "/content" on-main     :content)
				    ("Logout"          "/logout"  on-logout   :logout)))
		       ("Admin"    (("User List"       "/users"   on-users    :users)))
		       ("Help"     (("About"           "/content/about"))))
  "Setup website menu")

(defun start-tutorial ()
  ;; Here we add authorizations for content and editting content, not just
  ;; access to pages.
  (add-authorization '(:guest :member) '(:content-show-comments))
  (add-authorization '(:guest)         '(:login :signup))
  (add-authorization '(:member)        '(:logout
					 :change-password
				         :content-comment))
  (add-authorization '(:editor)        '(:content-edit))
  (add-authorization '(:admin)         '(:users :content-admin))
  ;; Setup database connection
  (when *sql-connection*
    (dbi:disconnect *sql-connection*))
  (let ((db-dir (format nil "~A~A" (asdf:system-source-directory :clog) "tut-32.db")))
    (setf *sql-connection* (dbi:connect :sqlite3 :database-name db-dir))
    (format t "Database location: ~A~%" db-dir))
  ;; Check if need to setup sample data
  (handler-case
      (dbi:fetch (dbi:execute (dbi:prepare *sql-connection* "select * from config")))
    (error ()
      (print "Create database and tables.")
      (create-base-tables *sql-connection*)
      ;; A main page was added, but let's also add an about page:
      (dbi:do-sql
	*sql-connection*
	(sql-insert* "content" `(:key        "about"
				 :title      "About Tutorial 32"
				 :value      "All about me."
				 :createdate (,*sqlite-timestamp*))))))
  ;; Setup clog
  (initialize 'on-main
	      :long-poll-first t
	      :extended-routing t
	      :boot-function (clog-web-meta
			      "clogpower.com - CLOG - the common lisp omnificent gui"))
  (clog-web-routes-from-menu *menu*)
  (open-browser))

;;
;; Look and Feel
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-site (body)
  "Setup the website, called on each url switch"
  ;; Initialize the clog-web environment
  (clog-web-initialize body)
  ;; Instantly reload other windows open on authentication change
  (set-on-authentication-change body (lambda (body)
				       (url-replace (location body) "/")))
  ;; Initialzie the clog-web-site environment
  (let ((profile (get-profile body *sql-connection*)))
    (create-web-site body
		     :settings '(:color-class  "w3-blue-gray"
				 :border-class ""
				 :signup-link  "/signup"
				 :login-link   "/login")
		     :profile profile
		     ;; We define the roles simply if logged out a :guest
		     ;; if logged in a :member and if username is admin
		     ;; a :member, :editor and :admin.
		     :roles (if profile
				(if (equalp "admin"
					    (getf profile :|username|))
				    '(:member :editor :admin)			    
				    '(:member))
				'(:guest))
		     :title "CLOG - The Common Lisp Omnificent GUI"
		     :footer "(c) 2022 David Botton"
		     :logo "/img/clog-liz.png")))

;;
;; URL Path Handlers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-login (body)
  (init-site body)
  (create-web-page
   body
   :login `(:menu      ,*menu*
	    :on-submit ,(lambda (obj)
			  (if (login body *sql-connection*
				     (name-value obj "username")
				     (name-value obj "password"))
			      (url-replace (location body) "/")
			      (clog-web-alert obj "Invalid" "The username and password are invalid."
					      :time-out 3
					      :place-top t))))
   :authorize t))

(defun on-logout (body)
  (logout body)
  (url-replace (location body) "/"))

(defun on-signup (body)
  (init-site body)
  (create-web-page body
		   :signup `(:menu    ,*menu*
			     :content ,(lambda (body)
					 (sign-up body *sql-connection*)))
		   :authorize t))

(defun on-main (body)
  (init-site body)
  (create-web-page body :index `(:menu    ,*menu*
				 :content ,(clog-web-content *sql-connection*
							     :comment-table "content"))))

(defun on-users (body)
  (init-site body)
  (create-web-page body :users
		   `(:menu    ,*menu*
		     :content ,(lambda (body)
				 (let ((users (dbi:fetch-all
					       (dbi:execute
						(dbi:prepare
						 *sql-connection*
						 "select * from users")))))
				   (dolist (user users)
				     (create-div body :content (getf user :|username|))))))
			:authorize t))

(defun on-new-pass (body)
  (init-site body)
  (create-web-page body
		   :change-password `(:menu    ,*menu*
				      :content ,(lambda (body)
						  (change-password body *sql-connection*)))
		   :authorize t))
