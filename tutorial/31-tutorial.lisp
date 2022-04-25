;; Demonstrates clog-web-site using a database oriented setup
;; using clog-web-dbi for users and clog-auth for authentication
;; and authorization

(defpackage #:clog-tut-31
  (:use #:cl #:clog #:clog-web #:clog-auth #:clog-web-dbi)
  (:export start-tutorial))

(in-package #:clog-tut-31)

;;
;; Setup website structure, database and CLOG
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *sql-connection* nil)

;; We use authorizations to control what menus appear if logged in or not.
;; We use the :authorize key on create-web-page to limit access as well
;; directly to a page when not using menus.
;; We use our menu with the convenient clog-web-routes-from-menu to
;; setup the routes from URLs to handlers:

                      ; Menu         Menu Item   URL       Handler   Actions Auth
(defparameter *menu* `(("Features" (("Login"     "/login"  on-login  :login)
				    ("Signup"    "/signup" on-signup :signup)
				    ("Main"      "/main"   on-main   :main)
				    ("Logout"    "/logout" on-logout :logout)))
		       ("Admin"    (("User List" "/users"  on-users  :users)))
		       ("Help"     (("About"     "/about"  on-about))))
  "Setup website menu")

(defun start-tutorial ()
  ;; Setup authorizations between roles and actions
  (add-authorization '(:guest) '(:login :signup :main))
  (add-authorization '(:member) '(:logout :main))
  (add-authorization '(:admin) '(:users))
  ;; Setup database connection
  (when *sql-connection*
    (dbi:disconnect *sql-connection*))
  (let ((db-dir (format nil "~A~A" (asdf:system-source-directory :clog) "tut-31.db")))
    (setf *sql-connection* (dbi:connect :sqlite3 :database-name db-dir))
    (format t "Database location: ~A~%" db-dir))
  ;; Check if need to setup sample data
  (handler-case
      (dbi:fetch (dbi:execute (dbi:prepare *sql-connection* "select * from config")))
    (error ()
      (print "Create database and tables.")
      (create-base-tables *sql-connection*)))
  ;; Setup clog, using long polling for web crawlers and some meta info
  (initialize 'on-main
	      :long-poll-first t
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
				 :border-class "")
		     :profile profile
		     :roles (if profile
				(if (equalp "admin"
					    (getf profile :|username|))
				    '(:member :admin)
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
	    :sign-up   "/signup"
	    :on-submit ,(lambda (obj)
			  (if (login body *sql-connection*
				     (name-value obj "username")
				     (name-value obj "password"))
			      ;; url-replace removes login from history stack
			      (url-replace (location body) "/main")
			      (clog-web-alert obj "Invalid" "The username and password are invalid."
					      :time-out 3
					      :place-top t))))
   ;; don't authorize use of page if logged in
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
		   ;; don't authorize use of page if logged in
		   :authorize t))

(defun on-main (body)
  (init-site body)
  (create-web-page body :main `(:menu    ,*menu*
				:content "I am the main page")))

(defun on-about (body)
  (init-site body)
  (create-web-page body :about `(:menu    ,*menu*
				 :content "About Me")))

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
		   ;; don't authorize use of page unless you are the admin
			:authorize t))
