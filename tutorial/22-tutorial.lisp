(defpackage #:clog-tut-22
  (:use #:cl #:clog #:clog-gui) ; For this tutorial we include clog-gui
  (:export start-tutorial))

(in-package :clog-tut-22)

;;; Demostrate a virtual desktop using CLOG GUI
(defun on-file-count (obj)
  (let ((win (create-gui-window obj :title "Count")))
    (dotimes (n 100)
      ;; window-content is the root element for the clog-gui
      ;; windows
      (create-div (window-content win) :content n))))

(defun on-file-with-menu (obj)
  (let ((win (create-gui-window obj :title "Window With Menu")))
    (let* ((menu  (create-gui-menu-bar (window-content win) :main-menu nil))
           (file  (create-gui-menu-drop-down menu :content "Menu 1"))
           (tmp   (create-gui-menu-item file :content "Count" :on-click 'on-file-count))
           (file  (create-gui-menu-drop-down menu :content "Menu 2"))
           (tmp   (create-gui-menu-item file :content "Count" :on-click 'on-file-count)))
      (declare (ignore tmp))
      (add-class menu "w3-small")
      (create-div (window-content win) :content "Some Text"))))

(defun on-file-browse (obj)
  (let ((win (create-gui-window obj :title "Browse")))
    (create-child (window-content win)
                  "<iframe style='width:100%;height:97%;' src='https://common-lisp.net/'></iframe>")))

(defun on-file-drawing (obj)
  (let* ((win (create-gui-window obj :title "Drawing"))
         (canvas (create-canvas (window-content win) :width 600 :height 400))
         (cx     (create-context2d canvas)))
    (set-border canvas :thin :solid :black)
    (setf (fill-style cx) :green)
    (fill-rect cx 10 10 150 100)
    (setf (fill-style cx) :blue
          (font-style cx) "bold 24px serif")
    (fill-text cx "Hello World" 10 150)
    (setf (fill-style cx) :red)
    (begin-path cx)
    (ellipse cx 200 200 50 7 0.78 0 6.29)
    (path-stroke cx)
    (path-fill cx)))

(defun on-file-movies (obj)
  (let* ((win  (create-gui-window obj :title "Movie"))
         (movie (create-video (window-content win)
                              :source "https://www.w3schools.com/html/mov_bbb.mp4")))
    (set-geometry movie :units "%" :width 100 :height 100)))

(defun on-file-on-top (obj)
  (let ((win (create-gui-window obj :title "Pin me!"
                                    :has-pinner nil
                                    :keep-on-top t
                                    :top 200
                                    :left 0
                                    :width 200
                                    :height 200)))
    (create-div (window-content win)
                :content "I am always on top")))

(defun on-file-pinned (obj)
  (let ((win (create-gui-window obj :title "Pin me!"
                                    :has-pinner t
                                    :keep-on-top nil
                                    :top 200
                                    :left 0
                                    :width 200
                                    :height 200)))
    (create-div (window-content win)
                :content "I can be pinned but do not stay on top. Just click the pin on window bar.")))

(defun on-file-pinned-on-top (obj)
  (let ((win (create-gui-window obj :title "Pin me!"
                                    :has-pinner t
                                    :keep-on-top t
                                    :top 200
                                    :left 0
                                    :width 200
                                    :height 200)))
    (create-div (window-content win)
                :content "I can be pinned and stay on top. Just click the pin on window bar.")))

(defun on-file-pop-tab (obj)
  (let ((pop (open-clog-popup obj)))
    (cond (pop ; pop is the body of the new browser tab/window
            (clog-gui-initialize pop)
            (add-class pop "w3-cyan")
            (create-div pop :content "I am a popup browser tab"))
          (t
            (alert-toast obj "Popup failure" "Unable to open popup tab")))))

(defun on-file-pop-win (obj)
  (let ((pop (open-clog-popup obj :specs "width=320,height=320")))
    (cond (pop ; pop is the body of the new browser tab/window
            (clog-gui-initialize pop )
            (add-class pop "w3-cyan")
            (create-div pop :content "I am a popup window when the browser allows it otherwise a tab"))
          (t
            (alert-toast obj "Popup failure" "Unable to open popup tab")))))

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

(defun on-dlg-form (obj)
  (form-dialog obj "Please enter your information."
               '(("Title" "title" :select (("Mr." "mr")
                                           ("Mrs." "mrs" :selected)
                                           ("Ms." "ms")
                                           ("Other" "other")))
                 ("Eye Color" "color" :radio (("Blue" "blue")
                                              ("Brown" "brown")
                                              ("Green" "green" :checked)
                                              ("Other" "other")))
                 ("Send Mail" "send-mail" :checkbox t)
                 ("Name" "name" :text "Real Name")
                 ("Address" "address")
                 ("City" "city")
                 ("State" "st")
                 ("Zip" "zip")
                 ("E-Mail" "email" :email))
               (lambda (results)
                 (alert-dialog obj results))
               :height 550))

(defun on-dlg-prompt (obj)
  (flet ((make-completion (selection &key (test #'search))
           (lambda (str)
             (remove-if-not (alexandria:curry test str) selection))))
    (let ((options '("white" "red" "orange" "yellow" "green" "blue"
                     "indigo" "violet" "brown" "black" "grey")))
      (prompt-dialog obj
                     (lambda (color) (alert-dialog obj color))
                     :title "Color Prompt"
                     :completion (make-completion options)
                     :validation (lambda (str)
                                   (find str options :test #'equal))))))

(defun on-toast-alert (obj)
  (alert-toast obj "Stop!" "To get rid of me, click the X. I have no time-out"))

(defun on-toast-warn (obj)
  (alert-toast obj "Warning!" "To get rid of me, click the X. I time-out in 5 seconds"
               :color-class "w3-yellow" :time-out 5))

(defun on-toast-success (obj)
  (alert-toast obj "Success!" "To get rid of me, click the X. I time-out in 2 seconds"
               :color-class "w3-green" :time-out 2))

(defun on-help-about (obj)
  (let* ((about (create-gui-window obj
                                   :title   "About"
                                   :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
                                         <center>CLOG</center>
                                         <center>The Common Lisp Omnificent GUI</center></div>
                                         <div><p><center>Tutorial 22</center>
                                         <center>(c) 2024 - David Botton</center></p></div>"
                                   :hidden  t
                                   :width   200
                                   :height  215)))
    (window-center about)
    (setf (visiblep about) t)
    (set-on-window-can-size about (lambda (obj)
                                    (declare (ignore obj))()))))

(defun on-maximize-window (obj)
  (when (current-window obj)
    (window-maximize (current-window obj))))

(defun on-normalize-window (obj)
  (when (current-window obj)
    (window-normalize (current-window obj))))

(defun on-new-window (body)
  (setf (title (html-document body)) "Tutorial 22")
  ;; For web oriented apps consider using the :client-movement option.
  ;; See clog-gui-window documentation.
  (clog-gui-initialize body)
  (enable-clog-popup) ; To allow browser popups
  (add-class body "w3-cyan")
  (let* ((menu  (create-gui-menu-bar body))
         (tmp   (create-gui-menu-icon menu :on-click 'on-help-about))
         (file  (create-gui-menu-drop-down menu :content "File"))
         (tmp   (create-gui-menu-item file :content "Count" :on-click 'on-file-count))
         (tmp   (create-gui-menu-item file :content "Window With Menu" :on-click 'on-file-with-menu))
         (tmp   (create-gui-menu-item file :content "Browse" :on-click 'on-file-browse))
         (tmp   (create-gui-menu-item file :content "Drawing" :on-click 'on-file-drawing))
         (tmp   (create-gui-menu-item file :content "Movie" :on-click 'on-file-movies))
         (tmp   (create-gui-menu-item file :content "Always on Top" :on-click 'on-file-on-top))
         (tmp   (create-gui-menu-item file :content "Pinned" :on-click 'on-file-pinned))
         (tmp   (create-gui-menu-item file :content "Pinned and on Always Top" :on-click 'on-file-pinned-on-top))
         (tmp   (create-gui-menu-item file :content "Popup Browser Tab" :on-click 'on-file-pop-tab))
         (tmp   (create-gui-menu-item file :content "Popup Browser Window" :on-click 'on-file-pop-win))
         (win   (create-gui-menu-drop-down menu :content "Window"))
         (tmp   (create-gui-menu-item win :content "Maximize" :on-click 'on-maximize-window))
         (tmp   (create-gui-menu-item win :content "Normalize" :on-click 'on-normalize-window))
         (tmp   (create-gui-menu-item win :content "Maximize All" :on-click 'maximize-all-windows))
         (tmp   (create-gui-menu-item win :content "Normalize All" :on-click 'normalize-all-windows))
         (tmp   (create-gui-menu-window-select win))
         (dlg   (create-gui-menu-drop-down menu :content "Dialogs"))
         (tmp   (create-gui-menu-item dlg :content "Alert Dialog Box" :on-click 'on-dlg-alert))
         (tmp   (create-gui-menu-item dlg :content "Input Dialog Box" :on-click 'on-dlg-input))
         (tmp   (create-gui-menu-item dlg :content "Confirm Dialog Box" :on-click 'on-dlg-confirm))
         (tmp   (create-gui-menu-item dlg :content "Prompt Dialog Box" :on-click 'on-dlg-prompt))
         (tmp   (create-gui-menu-item dlg :content "Form Dialog Box" :on-click 'on-dlg-form))
         (tmp   (create-gui-menu-item dlg :content "Server File Dialog Box" :on-click 'on-dlg-file))
         (tst   (create-gui-menu-drop-down menu :content "Toasts"))
         (tmp   (create-gui-menu-item tst :content "Alert Toast" :on-click 'on-toast-alert))
         (tmp   (create-gui-menu-item tst :content "Warning Toast" :on-click 'on-toast-warn))
         (tmp   (create-gui-menu-item tst :content "Success Toast" :on-click 'on-toast-success))
         (help  (create-gui-menu-drop-down menu :content "Help"))
         (tmp   (create-gui-menu-item help :content "About" :on-click 'on-help-about))
         (tmp   (create-gui-menu-full-screen menu)))
    (declare (ignore tmp)))
  ;; Alternatively with-clog-create can be used to declartively create the menu
  ;; see tutorial 33
  ;; (with-clog-create body
  ;;   (gui-menu-bar ()
  ;;              (gui-menu-icon (:on-click 'on-help-about))
  ;;              (gui-menu-drop-down (:content "File")
  ;;                                  (gui-menu-item (:content "Count" :on-click 'on-file-count))
  ;;                                  (gui-menu-item (:content "Browse" :on-click 'on-file-browse))
  ;;                                  (gui-menu-item (:content "Drawing" :on-click 'on-file-drawing))
  ;;                                  (gui-menu-item (:content "Movie" :on-click 'on-file-movies))
  ;;                                  (gui-menu-item (:content "Pinned" :on-click 'on-file-pinned)))
  ;;              (gui-menu-drop-down (:content "Window")
  ;;                                  (gui-menu-item (:content "Maximize All" :on-click 'maximize-all-windows))
  ;;                                  (gui-menu-item (:content "Normalize All" :on-click 'normalize-all-windows))
  ;;                                  (gui-menu-window-select ()))
  ;;              (gui-menu-drop-down (:content "Dialogs")
  ;;                                  (gui-menu-item (:content "Alert Dialog Box" :on-click 'on-dlg-alert))
  ;;                                  (gui-menu-item (:content "Input Dialog Box" :on-click 'on-dlg-input))
  ;;                                  (gui-menu-item (:content "Confirm Dialog Box" :on-click 'on-dlg-confirm))
  ;;                                  (gui-menu-item (:content "Form Dialog Box" :on-click 'on-dlg-form))
  ;;                                  (gui-menu-item (:content "Server File Dialog Box" :on-click 'on-dlg-file)))
  ;;              (gui-menu-drop-down (:content "Toasts")
  ;;                                  (gui-menu-item (:content "Alert Toast" :on-click 'on-toast-alert))
  ;;                                  (gui-menu-item (:content "Warning Toast" :on-click 'on-toast-warn))
  ;;                                  (gui-menu-item (:content "Success Toast" :on-click 'on-toast-success)))
  ;;              (gui-menu-drop-down (:content "Help")
  ;;                                  (gui-menu-item (:content "About" :on-click 'on-help-about)))
  ;;              (gui-menu-full-screen ())))
  (set-on-before-unload (window body) (lambda(obj)
                                        (declare (ignore obj))
                                        ;; return empty string to prevent nav off page
                                        "")))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
