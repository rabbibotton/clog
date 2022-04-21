;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-web-themes.lisp                                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Built in clog-web-site themese

(cl:in-package :clog-web)

(defsection @clog-web-themes (:title "CLOG Web Site Themes")
  "Built in themes"
  (default-theme  function))

;;;;;;;;;;;;;;;;;;;
;; default-theme ;;
;;;;;;;;;;;;;;;;;;;

(defun default-theme (body website page properties)
  "The default theme for clog-web-site.
Settings available:
  :menu-class - w3 color class for menu bar
Page properties:
  :menu - ((\"Menu Name\" ((\"Menu Item\" \"link\"))))
  :content"    
  (let ((sb (create-style-block body)))
    (add-style sb :element "a" '(("text-decoration" :none))))
  (let* ((row   (create-web-auto-row body))
	 (left  (create-web-auto-column row))
	 (right (create-web-auto-column row :vertical-align :middle)))
    (when (logo website)
      (set-geometry (create-img (create-a left
					  :link (url website))
				:url-src (logo website))
		    :height 75))
    (create-span (create-a right
			   :link (url website))
		 :content (title website)
		 :class "w3-xlarge w3-sans-serif"))
  (let ((menu  (create-web-menu-bar body :class "w3-card-4")))
    (when (getf (settings website) :menu-class)
      (add-class menu (getf (settings website) :menu-class)))
    (dolist (drop-down (getf properties :menu))
      (let ((drop (create-web-menu-drop-down menu
					     :content (first drop-down)
					     :class "w3-border")))
	(dolist (item (second drop-down))
	  (create-web-menu-item drop
				:content (first item)
				:link (second item))))))
  (create-br body)
  (let ((c (getf properties :content)))
    (when c
      (typecase c
	(string
	 (create-div body :content c))
	(function
	 (funcall c body))
	(t
	 (create-div body :content (format nil "~A" c))))))
  (when (eq page :login)
    (let* ((outter    (create-web-container body))
	   (form      (create-form outter))
	   (p1        (create-p form))
	   (l1        (create-label p1 :content "User Name"))
	   (user      (create-form-element p1 :text :name "username" :class "w3-input"))
	   (p2        (create-p form))
	   (l2        (create-label p2 :content "Password"))
	   (pass      (create-form-element p2 :password :name "password" :class "w3-input"))
	   (p3        (create-p form)))

      (declare (ignore l1 l2 p3))
      (setf (maximum-width outter) (unit :px 500))
      (setf (requiredp user) t)
      (setf (requiredp pass) t)
      (create-form-element form :submit :value "Submit"
					:class (format nil "~A ~A" "w3-button"
						       (getf (settings website)
							     :menu-class)))
      (set-on-submit form (getf properties :on-submit))
      (when (getf properties :sign-up)
	(create-a form :class "w3-right" :content "sign up" :link (getf properties :sign-up)))))
  (create-br body)
  (create-br body)
  (create-div body :content (format nil "~A" (footer website))))
