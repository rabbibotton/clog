;; Demonstrates clog-web-site from clog-web instant websites

(defpackage #:clog-tut-30
  (:use #:cl #:clog #:clog-web)
  (:export start-tutorial))

(in-package :clog-tut-30)

;; This is the menu structure for the default theme

(defparameter *menu* `(("Stuff" (("Link 1" "/link1")
				 ("Link 2" "/link2")))
		       ("Help"  (("About" "/about")))))

;; Init the site on every new page request
(defun init-site (body)
  (clog-web-initialize body)
  (create-web-site body
		   :settings '(:menu-class "w3-black")
		   :title "CLOG - The Common Lisp Omnificent GUI"
		   :footer "(c) 2022 David Botton"
		   :logo "/img/clog-liz.png"))

;; /
(defun on-main (body)
  (init-site body)
  (create-web-page body :main `(:menu    ,*menu*
				:content ,(lambda (body)
					    (create-span body :content "Main")))))

;; /about
(defun on-about (body)
  (init-site body)
  (create-web-page body :main `(:menu    ,*menu*
				:content "About Me")))

(defun start-tutorial ()
  ;; Initialize CLOG and the / url path
  (initialize 'on-main
	      ;; Use long polling technique so pages are crawled by google
	      :long-poll-first t
	      ;; Supply so meta info
	      :boot-function (clog-web-meta
			      "clogpower.com - CLOG - the common lisp omnificent gui")
	      :static-root (merge-pathnames "./www/"
					    (asdf:system-source-directory :cweb)))
  ;; Add /about
  (set-on-new-window 'on-about :path "/about")
  (open-browser))
