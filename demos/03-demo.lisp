;;; As this demo uses eval do not run over the internet.
;;; This app gives access to its users your local files.

(defpackage #:clog-demo-3
  (:use #:cl #:clog #:clog-gui)
  (:export start-demo))

(in-package :clog-demo-3)

(defclass app-data ()
  ((copy-buf
    :accessor copy-buf
    :initform ""
    :documentation "Copy buffer")))

(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun write-file (string outfile &key (action-if-exists :rename))
   (check-type action-if-exists (member nil :error :new-version :rename :rename-and-delete 
                                        :overwrite :append :supersede))
   (with-open-file (outstream outfile :direction :output :if-exists action-if-exists)
     (write-sequence string outstream)))

(defun capture-eval (form)
  (let ((result (make-array '(0) :element-type 'base-char
				 :fill-pointer 0 :adjustable t))
	(eval-result))
    (with-output-to-string (stream result)
      (let ((*standard-output* stream)
	    (*error-output* stream))
	(setf eval-result (eval (read-from-string (format nil "(progn ~A)" form))))))
    (format nil "~A~%=>~A~%" result eval-result)))    

(defun do-ide-file-new (obj)
  (let ((win (create-gui-window obj :title "New window"
				    :height 400
				    :width 650)))
    (set-on-window-size win (lambda (obj)
			      (js-execute obj
					  (format nil "editor_~A.resize()" (html-id win)))))
    (set-on-window-size-done win (lambda (obj)
				   (js-execute obj
					       (format nil "editor_~A.resize()" (html-id win)))))
    (create-child win
		  (format nil
			  "<script>
                            var editor_~A = ace.edit('~A-body');
                            editor_~A.setTheme('ace/theme/xcode');
                            editor_~A.session.setMode('ace/mode/lisp');
                            editor_~A.session.setTabSize(3);
                            editor_~A.focus();
                           </script>"
			  (html-id win) (html-id win)
			  (html-id win)
			  (html-id win)
			  (html-id win)
			  (html-id win)))))

(defun do-ide-file-open (obj)
  (server-file-dialog obj "Open..." "./"
		      (lambda (fname)
			(when fname
			  (do-ide-file-new obj)
			  (setf (window-title (current-window obj)) fname)
			  (js-execute obj
				      (format nil
					      "editor_~A.setValue('~A');editor_~A.moveCursorTo(0,0);"
					      (html-id (current-window obj))
					      (escape-string (read-file fname))
					      (html-id (current-window obj))))))))

(defun do-ide-file-save-as (obj)
  (let* ((cw  (current-window obj))
	 (dir (directory-namestring (window-title cw))))
    (when cw
      (server-file-dialog obj "Save As.." dir
			  (lambda (fname)
			    (window-focus cw)
			    (when fname
			      (setf (window-title cw) fname)
			      (write-file (js-query obj (format nil "editor_~A.getValue()"
								(html-id cw)))
					  fname)))
			  :initial-filename (when (equal (window-title cw) "New Window")
					      (window-title cw))))))

(defun do-ide-file-save (obj)
  (if (equalp (window-title (current-window obj)) "New Window")
      (do-ide-file-save-as obj)
      (let* ((cw     (current-window obj))
	     (fname  (window-title cw)))
	(write-file (js-query obj (format nil "editor_~A.getValue()"
					  (html-id cw)))
		    fname)
	(setf (window-title cw) "SAVED")
	(sleep 2)
	(setf (window-title cw) fname))))

(defun do-ide-edit-copy (obj)
  (let ((cw (current-window obj)))
    (when cw
      (let ((app (connection-data-item obj "app-data")))
	(setf (copy-buf app) (js-query obj
		    (format nil "editor_~A.execCommand('copy');~
                                 navigator.clipboard.writeText(editor_~A.getCopyText());~
                                 editor_~A.getCopyText();"
			    (html-id cw) (html-id cw) (html-id cw))))))))

(defun do-ide-edit-undo (obj)
  (let ((cw (current-window obj)))
    (when cw
      (do-ide-edit-copy obj)
      (js-execute obj (format nil "editor_~A.execCommand('undo')"
			      (html-id cw))))))
			      
(defun do-ide-edit-redo (obj)
  (let ((cw (current-window obj)))
    (when cw
      (js-execute obj (format nil "editor_~A.execCommand('redo')"
			      (html-id cw))))))

(defun do-ide-edit-cut (obj)
  (let ((cw (current-window obj)))
    (when cw
      (do-ide-edit-copy obj)
      (js-execute obj (format nil "editor_~A.execCommand('cut')"
			      (html-id cw))))))

(defun do-ide-edit-paste (obj)
  (let ((cw (current-window obj)))
    (when cw
      ;; Note this methods uses the global clip buffer and not (copy-buf app)
      ;; on copy and paste we set both the global and local buffer.
      (js-execute obj (format nil "navigator.clipboard.readText().then(function(text) {~
                                        editor_~A.execCommand('paste', text)~
                                     })"
			      (html-id cw))))))

(defun do-ide-lisp-eval-file (obj)
  (let ((cw (current-window obj)))
    (when cw
      (let* ((form-string (js-query obj (format nil "editor_~A.getValue()"
						(html-id (current-window obj)))))
	     (result      (capture-eval form-string)))
	
	(do-ide-file-new obj)
	(setf cw (current-window obj))
	(js-execute obj (format nil "editor_~A.setValue('~A');editor_~A.moveCursorTo(0,0);"
				(html-id cw)
				(escape-string result)
				(html-id cw)))))))

(defun do-ide-help-about (obj)
  (let ((about (create-gui-window obj
				  :title   "About"
				  :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
	                                 <center>CLOG</center>
	                                 <center>The Common Lisp Omnificent GUI</center></div>
			                 <div><p><center>Demo 3</center>
                                         <center>(c) 2021 - David Botton</center></p></div>"
				  :hidden  t
				  :width   200
				  :height  215)))
    (window-center about)
    (setf (visiblep about) t)
    (set-on-window-can-size about (lambda (obj)
				    (declare (ignore obj))()))))

(defun on-new-window (body)
  (set-html-on-close body "Connection Lost")
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app))
  (clog-gui-initialize body)
  ;; This is the Ace js code editor. This cdn works for most, if fails (getting
  ;; New as a blank window,etc) you can cd clog/static-files/ and run
  ;; git clone https://github.com/ajaxorg/ace-builds/
  ;; and uncomment this line and comment out the next:
  ;; (load-script (html-document body) "/ace-builds/src-noconflict/ace.js")
  (load-script (html-document body) "https://pagecdn.io/lib/ace/1.4.12/ace.js")
  (add-class body "w3-teal")
  (let* ((menu  (create-gui-menu-bar body))
	 (icon  (create-gui-menu-icon menu :on-click #'do-ide-help-about))
	 (file  (create-gui-menu-drop-down menu :content "File"))
	 (edit  (create-gui-menu-drop-down menu :content "Edit"))
	 (lisp  (create-gui-menu-drop-down menu :content "Lisp"))
	 (wind  (create-gui-menu-drop-down menu :content "Window"))
	 (help  (create-gui-menu-drop-down menu :content "Help")))
    (declare (ignore icon))
    (create-gui-menu-item file :content "New"           :on-click #'do-ide-file-new)
    (create-gui-menu-item file :content "Open"          :on-click #'do-ide-file-open)
    (create-gui-menu-item file :content "Save"          :on-click #'do-ide-file-save)
    (create-gui-menu-item file :content "Save As"       :on-click #'do-ide-file-save-as)
    (create-gui-menu-item edit :content "Undo"          :on-click #'do-ide-edit-undo)
    (create-gui-menu-item edit :content "Redo"          :on-click #'do-ide-edit-redo)
    (create-gui-menu-item edit :content "Copy"          :on-click #'do-ide-edit-copy)
    (create-gui-menu-item edit :content "Cut"           :on-click #'do-ide-edit-cut)
    (create-gui-menu-item edit :content "Paste"         :on-click #'do-ide-edit-paste)
    (create-gui-menu-item lisp :content "Eval File"     :on-click #'do-ide-lisp-eval-file)
    (create-gui-menu-item wind :content "Maximize All"  :on-click #'maximize-all-windows)
    (create-gui-menu-item wind :content "Normalize All" :on-click #'normalize-all-windows)
    (create-gui-menu-window-select wind)
    (create-gui-menu-item help :content "About"         :on-click #'do-ide-help-about)
    (create-gui-menu-full-screen menu))
  (set-on-before-unload (window body) (lambda(obj)
					(declare (ignore obj))
					;; return empty string to prevent nav off page
					""))
  (run body))

(defun start-demo ()
  "Start demo."
  (initialize #'on-new-window)
  (open-browser))
