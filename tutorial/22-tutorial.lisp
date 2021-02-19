(defpackage #:clog-user
  (:use #:cl #:clog #:clog-gui) ; For this tutorial we include clog-gui
  (:export start-tutorial))

(in-package :clog-user)

;; For web oriented apps consider using the :client-movement option.
;; See clog-gui-initialize documentation.

(defun on-file-count (obj)
  (let ((win (create-gui-window obj :title "Count")))
    (dotimes (n 100)
      ;; window-content is the root element for the clog-gui
      ;; windows
      (create-div (window-content win) :content n))))

(defun on-file-browse (obj)
  (let* ((win (create-gui-window obj :title "Browse")))
    (create-child (window-content win)
		  "<iframe width=100% height=98% src='https://common-lisp.net/'></iframe>")))

(defun on-file-drawing (obj)
  (let* ((win (create-gui-window obj :title "Drawing"))
	 (canvas (create-canvas (window-content win) :width 600 :height 400))
	 (cx     (create-context2d canvas)))
    (set-border canvas :thin :solid :black)    
    (fill-style cx :green)
    (fill-rect cx 10 10 150 100)
    (fill-style cx :blue)
    (font-style cx "bold 24px serif")
    (fill-text cx "Hello World" 10 150)
    (fill-style cx :red)
    (begin-path cx)
    (ellipse cx 200 200 50 7 0.78 0 6.29)
    (path-stroke cx)
    (path-fill cx)))

(defun on-file-movies (obj)
  (let* ((win  (create-gui-window obj :title "Movie"))
	 (movie (create-video (window-content win)
			      :source "https://www.w3schools.com/html/mov_bbb.mp4")))
    (setf (box-width movie) "100%")
    (setf (box-height movie) "100%")))

(defun on-file-pinned (obj)
  (let ((win (create-gui-window obj :title "Pinned"
				     :top 200
				     :left 0
				     :width 100
				     :height 100)))
    (flet ((can-not-do (obj)(declare (ignore obj))()))
      (set-on-window-can-maximize win #'can-not-do)
      (set-on-window-can-close win #'can-not-do)
      (set-on-window-can-size win #'can-not-do))
    (window-keep-on-top win)
    (create-div win :content "I am pinned")))    

(defun on-dlg-alert (obj)
  (alert-dialog obj "This is a modal alert box"))

(defun on-dlg-confirm (obj)
  (confirm-dialog obj "Shall we play a game?"		  
		(lambda (input)
		  (if input
		      (alert-dialog obj "How about Global Thermonuclear War.")
		      (alert-dialog obj "You are no fun!")))
		:ok-text "Yes" :cancel-text "No"))

(defun on-dlg-input (obj)
  (input-dialog obj "Would you like to play a game?"
		(lambda (input)
		  (alert-dialog obj input))))

(defun on-dlg-file (obj)
  (server-file-dialog obj "Server files" "./" (lambda (fname)
						(alert-dialog obj fname))))

(defun on-help-about (obj)
  (let* ((about (create-gui-window obj
				   :title   "About"
				   :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
	                                 <center>CLOG</center>
	                                 <center>The Common Lisp Omnificent GUI</center></div>
			                 <div><p><center>Tutorial 22</center>
                                         <center>(c) 2021 - David Botton</center></p></div>"
				   :width   200
				   :height  200)))
    (window-center about)
    (set-on-window-can-size about (lambda (obj)
				    (declare (ignore obj))()))))

(defun on-new-window (body)
  (setf (title (html-document body)) "Tutorial 22")  
  (clog-gui-initialize body)
  (add-class body "w3-cyan")  
  (let* ((menu  (create-gui-menu-bar body))
	 (tmp   (create-gui-menu-icon menu :on-click #'on-help-about))
	 (file  (create-gui-menu-drop-down menu :content "File"))
	 (tmp   (create-gui-menu-item file :content "Count" :on-click #'on-file-count))
	 (tmp   (create-gui-menu-item file :content "Browse" :on-click #'on-file-browse))
	 (tmp   (create-gui-menu-item file :content "Drawing" :on-click #'on-file-drawing))
	 (tmp   (create-gui-menu-item file :content "Movie" :on-click #'on-file-movies))
	 (tmp   (create-gui-menu-item file :content "Pinned" :on-click #'on-file-pinned))
	 (win   (create-gui-menu-drop-down menu :content "Window"))
	 (tmp   (create-gui-menu-item win :content "Maximize All" :on-click #'maximize-all-windows))
	 (tmp   (create-gui-menu-item win :content "Normalize All" :on-click #'normalize-all-windows))
	 (tmp   (create-gui-menu-window-select win))
	 (dlg   (create-gui-menu-drop-down menu :content "Dialogs"))
	 (tmp   (create-gui-menu-item dlg :content "Alert Dialog Box" :on-click #'on-dlg-alert))	 
	 (tmp   (create-gui-menu-item dlg :content "Input Dialog Box" :on-click #'on-dlg-input))
	 (tmp   (create-gui-menu-item dlg :content "Confirm Dialog Box" :on-click #'on-dlg-confirm))
	 (tmp   (create-gui-menu-item dlg :content "Server File Dialog Box" :on-click #'on-dlg-file))
	 (help  (create-gui-menu-drop-down menu :content "Help"))
	 (tmp   (create-gui-menu-item help :content "About" :on-click #'on-help-about))
	 (tmp   (create-gui-menu-full-screen menu)))
    (declare (ignore tmp)))
  (set-on-before-unload (window body) (lambda(obj)
					(declare (ignore obj))
					;; return empty string to prevent nav off page
					""))
  (run body))

(defun start-tutorial ()
  "Start turtorial."
  (initialize #'on-new-window)
  (open-browser))
