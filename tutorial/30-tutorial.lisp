;; Demonstrates clog-web-site from clog-web instant websites

(defpackage #:clog-tut-30
  (:use #:cl #:clog #:clog-web)
  (:export start-tutorial))

(in-package :clog-tut-30)


;; Site setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;l;;

(defun init-site (body)
  (clog-web-initialize body)
  (create-web-site body
		   ;; use the default theme
		   :theme 'clog-web:default-theme
		   ;; theme settings - in this case w3.css color of menu bar
		   :settings '(:menu-class "w3-black")
		   :title "CLOG - The Common Lisp Omnificent GUI"
		   :footer "(c) 2022 David Botton"
		   :logo "/img/clog-liz.png"))

;; This is the menu structure
(defparameter *menu* `(("Content" (("Home"                "/"       on-main)
				   ("Content from Lambda" "/lambda" on-lambda)
				   ("Content from File"   "/readme" on-readme)))
		       ("Help"    (("About"               "/about"  on-about)))))

;; Page handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; / -  a simple content string
(defun on-main (body)
  ;; We call init-site on every page to load our theme and settings
  (init-site body)
  (create-web-page body :main `(:menu    ,*menu*
				:content "<b>Welcome to tutorial 30</b><p>Any HTML works!")))

;; /readme - get content from a text file
(defun on-readme (body)
  (init-site body)
  (let ((readme (alexandria:read-file-into-string
		 (format nil "~A~A" (asdf:system-source-directory :clog) "README.md"))))
    (create-web-page body :main `(:menu    ,*menu*
				  :content ,(format nil "<pre>~A</pre>" readme)))))

;; /lambda - use a function to output to the page content
(defun on-lambda (body)
  (init-site body)
  (create-web-page body :main `(:menu    ,*menu*
				:content ,(lambda (obj)
					    (create-div obj :content "I am in the content area")))))

;; /about
(defun on-about (body)
  (init-site body)
  (create-web-page body :main `(:menu    ,*menu*
				:content "About Me")))

;; Start the webserver
(defun start-tutorial ()
  ;; Initialize CLOG and the / url path (since / in our menu could just be nil)
  (initialize 'on-main
	      ;; Use long polling technique so pages are crawled by google
	      :long-poll-first t
	      ;; Supply some meta info
	      :boot-function (clog-web-meta
			      "clogpower.com - CLOG - the common lisp omnificent gui"))
  ;; clog web helper to set up routes in menu
  (clog-web-routes-from-menu *menu*)
  (open-browser))
