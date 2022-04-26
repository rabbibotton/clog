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
  :color-class   - w3 color class for menu bars and buttons
  :border-class  - w3 border
  :text-class    - w3 text color class
  :username-link - link when clicking on username (default /logout)
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
    (when (getf (settings website) :color-class)
      (add-class menu (getf (settings website) :color-class)))
    (dolist (drop-down (getf properties :menu))
      (let ((drop  (create-web-menu-drop-down menu
					      :content (first drop-down)
					      :class "w3-border"))
	    (count 0))
	(dolist (item (second drop-down))
	  (when (or (and (fourth item)
			 (clog-auth:is-authorized-p (roles website)
						    (fourth item)))
		    (eq (fourth item) nil))
	    (incf count)
	    (create-web-menu-item drop
				  :content (first item)
				  :link (second item))))
	(when (eql count 0)
	  (destroy (parent-element drop)))))
    (when (getf (profile website) :|username|)
      (create-web-menu-item menu :class "w3-right"
				 :content (getf (profile website) :|username|)
				 :link (if (getf (settings website) :username-link)
					   (getf (settings website) :username-link)
					   "/logout"))))
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
	   (t-class   (if (getf (settings website) :text-class)
			  (getf (settings website) :text-class)
			  ""))
	   (b-class   (if (getf (settings website) :border-class)
			  (getf (settings website) :border-class)
			  ""))
	   (p1        (create-p form))
	   (l1        (create-label p1 :content "User Name"
				       :class t-class))
	   (user      (create-form-element p1 :text
					   :name "username"
					   :class (format nil "w3-input ~A" b-class)))
	   (p2        (create-p form))
	   (l2        (create-label p2 :content "Password"
				       :class t-class))
	   (pass      (create-form-element p2 :password
					   :name "password"
					   :class (format nil "w3-input ~A" b-class)))
	   (p3        (create-p form)))

      (declare (ignore l1 l2 p3))
      (setf (maximum-width outter) (unit :px 500))
      (setf (requiredp user) t)
      (setf (requiredp pass) t)
      (create-form-element form :submit :value "Submit"
					:class (format nil "~A ~A" "w3-button"
						       (getf (settings website)
							     :color-class)))
      (set-on-submit form (getf properties :on-submit))
      (when (getf properties :sign-up)
	(create-a form :class "w3-right" :content "sign up" :link (getf properties :sign-up)))))
  (create-br body)
  (create-br body)
  (create-div body :content (format nil "~A" (footer website))))
