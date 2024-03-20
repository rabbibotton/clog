(in-package :clog-tools)

;; Local file utilities

(defun read-file (infile &key clog-obj)
  "Read local file named INFILE"
  (handler-case
      (with-open-file (instream infile :direction :input :if-does-not-exist nil)
	(when instream
	  (let* ((len    (file-length instream))
		 (string (make-string len))
		 (pos    (read-sequence string instream)))
            (subseq string 0 pos))))
    (error (condition)
      (if clog-obj
	  (alert-toast clog-obj "File Error" (format nil "Error: ~A" condition))
	  (format t "Error: ~A" condition)))))

(defun write-file (string outfile &key clog-obj (action-if-exists :rename))
  "Write local file named OUTFILE"
   (check-type action-if-exists (member nil :error :new-version :rename :rename-and-delete
                                            :overwrite :append :supersede))
  (handler-case
      (with-open-file (outstream outfile :direction :output :if-exists action-if-exists)
	(when outstream
	  (write-sequence string outstream)))
    (error (condition)
      (if clog-obj
	  (alert-toast clog-obj "File Error" (format nil "Error: ~A" condition))
	  (format t "Error: ~A" condition)))))

(defun on-open-file-ext (obj &key open-file popup)
  (open-window (window (connection-body obj))
               (if open-file
                   (format nil "/source-editor?open-file=~A"
                           open-file)
                   "/source-editor")
               :specs (if popup
                          "width=645,height-430"
                          "")
               :name "_blank"))

(defun on-open-file (obj &key open-file
                           (title "New Source Editor")
                           text
                           (title-class "w3-black")
                           maximized)
  "Open a new text editor"
  (unless (window-to-top-by-title obj open-file)
    (let* ((app (connection-data-item obj "builder-app-data"))
           (win (create-gui-window obj :title title
                                       :title-class title-class
                                       :width 645 :height 430
                                       :client-movement *client-side-movement*))
           (box (create-panel-box-layout (window-content win)
                                         :left-width 0 :right-width 0
                                         :top-height 33 :bottom-height 0))
           (tool-bar  (create-div (top-panel box) :class "w3-center"))
           (btn-class "w3-button w3-white w3-border w3-border-black w3-ripple")
           (btn-copy  (create-img tool-bar :alt-text "copy"     :url-src img-btn-copy  :class btn-class))
           (btn-paste (create-img tool-bar :alt-text "paste"    :url-src img-btn-paste :class btn-class))
           (btn-cut   (create-img tool-bar :alt-text "cut"      :url-src img-btn-cut   :class btn-class))
           (btn-del   (create-img tool-bar :alt-text "delete"   :url-src img-btn-del   :class btn-class))
           (btn-undo  (create-img tool-bar :alt-text "undo"     :url-src img-btn-undo  :class btn-class))
           (btn-redo  (create-img tool-bar :alt-text "redo"     :url-src img-btn-redo  :class btn-class))
           (btn-save  (create-img tool-bar :alt-text "save"     :url-src img-btn-save  :class btn-class))
           (btn-load  (create-img tool-bar :alt-text "load"     :url-src img-btn-load  :class btn-class))
           (spacer1   (create-span tool-bar :content "&nbsp;"))
           (btn-efrm  (create-button tool-bar :content "Eval Form" :class (format nil "w3-tiny ~A" btn-class)))
           (btn-esel  (create-button tool-bar :content "Eval Sel"  :class (format nil "w3-tiny ~A" btn-class)))
           (btn-test  (create-button tool-bar :content "Eval All"  :class (format nil "w3-tiny ~A" btn-class)))
           (spacer2   (create-span tool-bar :content "&nbsp;&nbsp;"))
           (btn-help  (create-span tool-bar :content "?" :class "w3-tiny w3-ripple"))
           (content   (center-panel box))
           (pac-line  (create-form-element content :text :class "w3-black"))
           (ace       (clog-ace:create-clog-ace-element content))
           (status    (create-div content :class "w3-tiny w3-border"))
           (lisp-file t)
           (is-dirty  nil)
           (last-date nil)
           (file-name ""))
      (declare (ignore spacer1 spacer2))
      (when maximized
        (window-maximize win))
      (when text
        (setf (text-value ace) text))
      (set-on-window-focus win
                           (lambda (obj)
                             (declare (ignore obj))
                             (if lisp-file
                                 (setf (current-editor-is-lisp app) (text-value pac-line))
                                 (setf (current-editor-is-lisp app) nil))))
      (add-class tool-bar title-class)
      (setf (advisory-title btn-paste) "paste")
      (setf (advisory-title btn-cut) "cut")
      (setf (advisory-title btn-del) "delete")
      (setf (advisory-title btn-undo) "undo")
      (setf (advisory-title btn-redo) "redo")
      (setf (advisory-title btn-save) "save  - shift-click save as...")
      (setf (advisory-title btn-load) "load")
      (setf (advisory-title btn-efrm) "evaluate form")
      (setf (advisory-title btn-esel) "evaluate selection")
      (setf (advisory-title btn-test) "evaluate")
      (setf (height btn-copy) "12px")
      (setf (height btn-paste) "12px")
      (setf (height btn-cut) "12px")
      (setf (height btn-del) "12px")
      (setf (height btn-undo) "12px")
      (setf (height btn-redo) "12px")
      (setf (height btn-save) "12px")
      (setf (height btn-load) "12px")
      (setf (height btn-efrm) "12px")
      (setf (height btn-esel) "12px")
      (setf (height btn-test) "12px")
      (setf (height btn-help) "12px")
      (setf (width btn-efrm) "43px")
      (setf (width btn-esel) "43px")
      (setf (width btn-test) "43px")
      (setf (positioning ace) :absolute)
      (setf (positioning status) :absolute)
      (set-geometry pac-line :units "" :top "20px" :left "0px"
                             :right "0px" :height "22px" :width "100%")
      (setf (place-holder pac-line) "Current Package")
      (setf (text-value pac-line) "clog-user")
      (setf (current-editor-is-lisp app) "clog-user")
      (set-geometry ace :units "" :width "" :height ""
                        :top "22px" :bottom "20px" :left "0px" :right "0px")
      (clog-ace:resize ace)
      (set-geometry status :units "" :width "" :height "20px"
                           :bottom "0px" :left "0px" :right "0px")
      (setup-lisp-ace ace status)
      (set-on-click btn-help
                    (lambda (obj)
                      (declare (ignore obj))
                      (alert-dialog win
                                    "<table>
<tr><td>cmd/alt-,</td><td>Configure editor</td></tr>
<tr><td>cmd/alt-.</td><td> Launch system browser</td></tr>
<tr><td>cmd/alt-[</td><td> Evaluate form</td></tr>
<tr><td>cmd/ctl-s</td><td> Save</td></tr>
<tr><td>ctl-=</td><td>Expand region</td></tr>
<tr><td>opt/alt-m</td><td>Macroexpand</td></tr>
</table><p><a target='_blank' href='https://github.com/ajaxorg/ace/wiki/Default-Keyboard-Shortcuts'>Default Keybindings</a>"
                                    :width 400 :height 300
                                    :title "Help")))
      (set-on-window-size-done win
                               (lambda (obj)
                                 (declare (ignore obj))
                                 (clog-ace:resize ace)))
      (flet ((open-file-name (fname)
               (window-focus win)
               (handler-case
                   (when fname
                     (setf last-date (file-write-date fname))
                     (setf file-name fname)
                     (setf (window-title win) fname)
                     (let ((c (or (read-file fname :clog-obj obj) "")))
                       (cond ((or (equalp (pathname-type fname) "lisp")
                                  (equalp (pathname-type fname) "asd"))
                              (setf (clog-ace:mode ace) "ace/mode/lisp")
                              (setf (text-value pac-line) (get-package-from-string c))
                              (setf lisp-file t)
                              (setf (current-editor-is-lisp app) (text-value pac-line)))
                             (t
                              (setf lisp-file nil)
                              (setf (current-editor-is-lisp app) nil)
                              (setf (clog-ace:mode ace) (clog-ace:get-mode-from-extension ace fname))))
                       (setf (clog-ace:text-value ace) c)))
                 (error (condition)
	           (alert-toast obj "File Error" (format nil "Error: ~A" condition))
	           (format t "Error: ~A" condition)))))
        (when (and open-file
                   (not (equalp open-file " ")))
          (open-file-name open-file))
        (set-on-click btn-load (lambda (obj)
                                 (server-file-dialog obj "Load Source" (directory-namestring (if (equal file-name "")
                                                                                                 (current-project-dir app)
                                                                                                 file-name))
                                                     (lambda (fname)
                                                       (open-file-name fname)
                                                       (setf is-dirty nil))))))
      (set-on-input ace (lambda (obj)
                          (declare (ignore obj))
                          (setf is-dirty t)))
      (set-on-event ace "clog-save-ace"
                    (lambda (obj)
                      (unless (equal file-name "")
                        (add-class btn-save "w3-animate-top")
                        (write-file (text-value ace) file-name :clog-obj obj)
                        (sleep .5)
                        (remove-class btn-save "w3-animate-top"))))
      (flet ((save (obj data)
               (cond ((or (equal file-name "")
                          (getf data :shift-key))
                      (server-file-dialog obj "Save Source As.." (if (equal file-name "")
                                                                     (current-project-dir app)
                                                                     file-name)
                                          (lambda (fname)
                                            (window-focus win)
                                            (when fname
                                              (setf file-name fname)
                                              (add-class btn-save "w3-animate-top")
                                              (write-file (text-value ace) fname :clog-obj obj)
                                              (setf last-date (file-write-date fname))
                                              (sleep .5)
                                              (remove-class btn-save "w3-animate-top"))
                                            :initial-filename file-name)))
                     (t
                      (cond ((eql last-date (file-write-date file-name))
                             (add-class btn-save "w3-animate-top")
                             (write-file (text-value ace) file-name :clog-obj obj)
                             (setf last-date (file-write-date file-name))
                             (sleep .5)
                             (remove-class btn-save "w3-animate-top"))
                            (t
                             (confirm-dialog obj "File changed on file system. Save?"
                                             (lambda (result)
                                               (when result
                                                 (add-class btn-save "w3-animate-top")
                                                 (write-file (text-value ace) file-name :clog-obj obj)
                                                 (setf last-date (file-write-date file-name))
                                                 (sleep .5)
                                                 (remove-class btn-save "w3-animate-top"))))))))))
        (set-on-window-can-close win
                                 (lambda (obj)
                                   (cond (is-dirty
                                          (confirm-dialog obj "Save File?"
                                                          (lambda (result)
                                                            (setf is-dirty nil)
                                                            (when result
                                                              (save obj nil))
                                                            (window-close win))
                                                          :ok-text "Yes" :cancel-text "No")
                                          nil)
                                         (t
                                          t))))
        (set-on-mouse-click btn-save
                            (lambda (obj data)
                              (save obj data)
                              (setf is-dirty nil))))
      (set-on-click btn-copy (lambda (obj)
                               (declare (ignore obj))
                               (clog-ace:clipboard-copy ace)))
      (set-on-click btn-cut (lambda (obj)
                              (declare (ignore obj))
                              (clog-ace:clipboard-cut ace)))
      (set-on-click btn-paste (lambda (obj)
                                (declare (ignore obj))
                                (clog-ace:clipboard-paste ace)))
      (set-on-click btn-del (lambda (obj)
                              (declare (ignore obj))
                              (clog-ace:execute-command ace "del")))
      (set-on-click btn-undo (lambda (obj)
                               (declare (ignore obj))
                               (clog-ace:execute-command ace "undo")))
      (set-on-click btn-redo (lambda (obj)
                               (declare (ignore obj))
                               (clog-ace:execute-command ace "redo")))
      (set-on-click btn-efrm (lambda (obj)
                               (let ((p  (parse-integer
                                          (js-query obj
                                                    (format nil "~A.session.doc.positionToIndex (~A.selection.getCursor(), 0);"
                                                            (clog-ace::js-ace ace)
                                                            (clog-ace::js-ace ace)))
                                          :junk-allowed t))
                                     (tv (text-value ace))
                                     (lf nil)
                                     (cp 0))
                                 (loop
                                   (setf (values lf cp) (read-from-string tv nil nil :start cp))
                                   (unless lf (return nil))
                                   (when (> cp p) (return lf)))
                                 (when lf
                                   (let ((result (capture-eval lf
                                                               :clog-obj (connection-body obj)
                                                               :eval-in-package (text-value pac-line))))
                                     (on-open-file obj :title-class "w3-blue" :title "form eval" :text result))))))
      (set-on-click btn-esel (lambda (obj)
                               (let ((val (clog-ace:selected-text ace)))
                                 (unless (equal val "")
                                   (let ((result (capture-eval val :clog-obj obj
                                                                   :eval-in-package (text-value pac-line))))
                                     (on-open-file obj :title-class "w3-blue" :title "selection eval" :text result))))))

      (set-on-click btn-test (lambda (obj)
                               (let ((val (text-value ace)))
                                 (unless (equal val "")
                                   (let ((result (capture-eval val :clog-obj obj
                                                                   :eval-in-package (text-value pac-line))))
                                     (on-open-file obj :title-class "w3-blue" :title "file eval" :text result)))))))))
