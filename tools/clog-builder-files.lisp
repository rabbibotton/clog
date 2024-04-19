(in-package :clog-tools)

;; Local file utilities

(defun read-file (infile &key clog-obj if-does-not-exist)
  "Read local file named INFILE"
  (handler-case
      (with-open-file (instream infile :direction :input :if-does-not-exist if-does-not-exist)
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
  (if (and *open-external-with-emacs* open-file)
      (swank:ed-in-emacs open-file)
      (let ((win (window-to-top-by-title obj open-file)))
        (if win
            (window-focus win)
            (if *open-external-using-clog-popups*
                (let ((pop (open-clog-popup obj
                                            :specs (if (or popup *open-external-source-in-popup*)
                                                       "width=640,height=480"
                                                       "")
                                            :name "_blank")))
                  (if pop
                      (let ((app (connection-data-item obj "builder-app-data")))
                        (setf (connection-data-item pop "builder-app-data") app)
                        (set-html-on-close pop "Connection Lost")
                        (clog-gui-initialize pop :parent-desktop-obj obj)
                        (add-class pop *builder-window-desktop-class*)
                        (if open-file
                            (setf (title (html-document pop)) (file-namestring open-file))
                            (setf (title (html-document pop)) "CLOG Builder Source Editor"))
                        (on-open-file pop :open-file open-file :maximized t))
                      (on-open-file obj :open-file open-file)))
                (open-window (window (connection-body obj))
                             (if open-file
                                 (format nil "/source-editor?open-file=~A"
                                         open-file)
                                 "/source-editor?open-file=%20")
                             :specs (if (or popup *open-external-source-in-popup*)
                                        "width=800,height=600"
                                        "")
                             :name "_blank"))))))

(defun on-open-file (obj &key open-file
                     (title "New Source Editor")
                     text
                     (title-class *builder-title-class*)
                     lisp-package
                     regex
                     is-console
                     (editor-use-console-for-evals *editor-use-console-for-evals*)
                     maximized)
  "Open a new text editor"
  (let ((win (window-to-top-by-title obj open-file)))
    (when win
      (let ((pop (connection-data-item obj "clog-popup")))
        (when pop
          (close-window pop)
          (window-focus win)))
      (when regex
        (js-execute win (format nil "~A.find('~A',{caseSensitive:false,regExp:true})"
                                (clog-ace::js-ace (window-param win)) regex)))
      win)
    (unless win
      (let* ((app (connection-data-item obj "builder-app-data"))
             (*menu-bar-class*           *builder-menu-bar-class*)
             (*menu-bar-drop-down-class* *builder-menu-bar-drop-down-class*)
             (*menu-item-class*          *builder-menu-item-class*)
             (*menu-window-select-class* *builder-menu-window-select-class*)
             (*default-title-class*      *builder-title-class*)
             (*default-border-class*     *builder-border-class*)
             (win (create-gui-window obj :title title
                                     :title-class title-class
                                     :width 700 :height 480
                                     :client-movement *client-side-movement*))
             (box (create-panel-box-layout (window-content win)
                                           :left-width 0 :right-width 0
                                           :top-height 66 :bottom-height 0))
             (menu     (create-gui-menu-bar (top-panel box) :main-menu nil))
             (m-file   (create-gui-menu-drop-down menu :content "File"))
             (m-load   (create-gui-menu-item m-file :content "load"))
             (m-save   (create-gui-menu-item m-file :content "save (cmd/ctrl-s)"))
             (m-saveas (create-gui-menu-item m-file :content "save as.."))
             (m-revert (create-gui-menu-item m-file :content "revert"))
             (m-emacs  (unless (in-clog-popup-p obj)
                         (create-gui-menu-item m-file :content "open in emacs")))
             (m-ntab   (unless (in-clog-popup-p obj)
                         (create-gui-menu-item m-file :content "open in new tab")))
             (m-edit   (create-gui-menu-drop-down menu :content "Edit"))
             (m-undo   (create-gui-menu-item m-edit :content "undo (cmd/ctrl-z)"))
             (m-redo   (create-gui-menu-item m-edit :content "redo (shift cmd/ctrl-z)"))
             (m-copy   (create-gui-menu-item m-edit :content "copy (cmd/ctrl-c)"))
             (m-paste  (create-gui-menu-item m-edit :content "paste (cmd/ctrl-v)"))
             (m-cut    (create-gui-menu-item m-edit :content "cut (cmd/ctrl-x)"))
             (m-del    (create-gui-menu-item m-edit :content "delete (del)"))
             (m-lisp   (create-gui-menu-drop-down menu :content "Lisp"))
             (m-efrm   (create-gui-menu-item m-lisp :content "evaluate form (cmd/alt-[)"))
             (m-esel   (create-gui-menu-item m-lisp :content "evaluate selection"))
             (m-test   (create-gui-menu-item m-lisp :content "evaluate all"))
             (m-brwsp  (create-gui-menu-item m-lisp :content "system browse at point (cmd/alt-.)"))
             (m-brws   (create-gui-menu-item m-lisp :content "system browse selection"))
             (m-desc   (create-gui-menu-item m-lisp :content "describe selection"))
             (m-doc    (create-gui-menu-item m-lisp :content "documentation on selection"))
             (m-apro   (create-gui-menu-item m-lisp :content "apropos on selection"))
             (m-pprt   (create-gui-menu-item m-lisp :content "adjust tabs at point (ctrl/alt-t)"))
             (m-ppr    (create-gui-menu-item m-lisp :content "adjust tabs file"))
             (m-pprs   (create-gui-menu-item m-lisp :content "adjust tabs selection"))
             (m-help   (create-gui-menu-drop-down menu :content "Help"))
             (m-helpk  (create-gui-menu-item m-help :content "keyboard help"))
             (tool-bar  (create-div (top-panel box) :class title-class))
             (btn-class *builder-icons-class*)
             (btn-copy  (create-img tool-bar :alt-text "copy"     :url-src img-btn-copy  :class btn-class))
             (btn-paste (create-img tool-bar :alt-text "paste"    :url-src img-btn-paste :class btn-class))
             (btn-cut   (create-img tool-bar :alt-text "cut"      :url-src img-btn-cut   :class btn-class))
             (btn-del   (create-img tool-bar :alt-text "delete"   :url-src img-btn-del   :class btn-class))
             (btn-undo  (create-img tool-bar :alt-text "undo"     :url-src img-btn-undo  :class btn-class))
             (btn-redo  (create-img tool-bar :alt-text "redo"     :url-src img-btn-redo  :class btn-class))
             (btn-save  (create-img tool-bar :alt-text "save"     :url-src img-btn-save  :class btn-class))
             (btn-load  (create-img tool-bar :alt-text "load"     :url-src img-btn-load  :class btn-class))
             (spacer1   (create-span tool-bar :content "&nbsp;"))
             (btn-efrm  (create-button tool-bar :content "Eval Form"  :class (format nil "w3-tiny ~A" btn-class)))
             (btn-esel  (create-button tool-bar :content "Eval Sel"   :class (format nil "w3-tiny ~A" btn-class)))
             (btn-test  (create-button tool-bar :content "Eval All"   :class (format nil "w3-tiny ~A" btn-class)))
             (btn-brws  (create-button tool-bar :content "Browse"     :class (format nil "w3-tiny ~A" btn-class)))
             (spacer2   (create-span tool-bar :content "&nbsp;&nbsp;"))
             (btn-help  (create-span tool-bar :content "?" :class "w3-tiny w3-ripple"))
             (content   (center-panel box))
             (pac-line  (create-form-element content :text :class *builder-package-class*))
             (ace       (clog-ace:create-clog-ace-element content))
             (status    (create-div content :class *builder-status-bar-class*))
             (lisp-file t)
             (is-dirty  nil)
             (last-date nil)
             (file-name (or open-file
                            "")))
        (declare (ignore spacer1 spacer2))
        (setf (window-param win) ace)
        (add-class menu "w3-small")
        (setf (overflow (top-panel box)) :visible) ; let menus leave the top panel
        (setf (z-index m-file) 10) ; fix for ace editor gutter overlapping menu
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
        (setf (advisory-title btn-brws) "system browse at point")
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
        (setf (height btn-brws) "12px")
        (setf (height btn-help) "12px")
        (setf (width btn-efrm) "43px")
        (setf (width btn-esel) "43px")
        (setf (width btn-test) "43px")
        (setf (positioning ace) :absolute)
        (setf (positioning status) :absolute)
        (set-geometry pac-line :units "" :top "20px" :left "0px"
                      :right "0px" :height "22px" :width "100%")
        (setf (place-holder pac-line) "Current Package")
        (if lisp-package
            (setf (text-value pac-line) lisp-package)
            (setf (text-value pac-line) "clog-user"))
        (setf (current-editor-is-lisp app) "clog-user")
        (set-geometry ace :units "" :width "" :height ""
                      :top "22px" :bottom "20px" :left "0px" :right "0px")
        (clog-ace:resize ace)
        (set-geometry status :units "" :width "" :height "20px"
                      :bottom "0px" :left "0px" :right "0px")
        (setup-lisp-ace ace status)
        (when is-console
          (setf (clog-ace:mode ace) "ace/mode/plain_text")
          (clog-ace:set-auto-completion ace nil)
          (set-on-change ace nil))
        (labels ((on-help (obj)
                   (declare (ignore obj))
                   (alert-dialog win
                                 "<table>
  <tr><td>cmd/ctrl-,</td><td>Configure editor</td></tr>
  <tr><td>F1</td><td>Command Palette</td></tr>
  <tr><td>cmd/alt-.</td><td>Launch system browser</td></tr>
  <tr><td>cmd/alt-[</td><td>Evaluate form</td></tr>
  <tr><td>cmd/ctrl-s</td><td>Save</td></tr>
  <tr><td>ctrl/alt-t</td><td>Adjust tabs at cursor</td></tr>
  <tr><td>ctrl/alt-=</td><td>Expand region</td></tr>
  <tr><td>opt/alt-m</td><td>Macroexpand</td></tr>
  </table><p><a target='_blank' href='https://github.com/ajaxorg/ace/wiki/Default-Keyboard-Shortcuts'>Default Keybindings</a>"
                                 :width 400 :height 400
                                 :title "Help")))
          (set-on-click btn-help #'on-help)
          (set-on-click m-helpk #'on-help))
        (set-on-window-size-done win
                                 (lambda (obj)
                                   (declare (ignore obj))
                                   (clog-ace:resize ace)))
        (labels ((set-is-dirty (status)
                   (cond (status
                           (setf is-dirty t)
                           (set-outline btn-save :yellow :solid :thin))
                         (t
                           (setf is-dirty nil)
                           (set-outline btn-save :green :solid :thin))))
                 (open-file-name (fname)
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
                       (unless text
                         (alert-toast obj "File Error" (format nil "Error: ~A" condition))
                         (format t "Error: ~A" condition)))))
                 (load-file (obj)
                   (server-file-dialog obj "Load Source" (directory-namestring (if (equal file-name "")
                                                                                   (current-project-dir app)
                                                                                   file-name))
                                       (lambda (fname)
                                         (open-file-name fname)
                                         (set-is-dirty nil)))))
          (when (and open-file
                     (not (equalp open-file " "))
                     (not (equalp open-file "")))
            (open-file-name open-file))
          (when regex
            (js-execute obj (format nil "~A.find('~A',{caseSensitive:false,regExp:true})"
                                    (clog-ace::js-ace ace) regex)))
          (set-on-click btn-load (lambda (obj) (load-file obj)))
          (set-on-click m-load (lambda (obj) (load-file obj)))
          (set-on-click m-revert (lambda (obj)
                                   (declare (ignore obj))
                                   (set-is-dirty nil)
                                   (open-file-name file-name)))
        (set-on-input ace (lambda (obj)
                            (declare (ignore obj))
                            (set-is-dirty t)))
        (set-on-event ace "clog-save-ace"
                      (lambda (obj)
                        (unless (equal file-name "")
                          (add-class btn-save "w3-animate-top")
                          (write-file (text-value ace) file-name :clog-obj obj)
                          (set-is-dirty nil)
                          (setf last-date (file-write-date file-name))
                          (sleep .5)
                          (remove-class btn-save "w3-animate-top"))))
        (flet ((save (obj data &key save-as)
                 (cond ((or (equal file-name "")
                            (getf data :shift-key)
                            save-as)
                         (server-file-dialog obj "Save Source As.." (if (equal file-name "")
                                                                        (current-project-dir app)
                                                                        file-name)
                                             (lambda (fname)
                                               (window-focus win)
                                               (when fname
                                                 (setf file-name fname)
                                                 (setf (window-title win) fname)
                                                 (add-class btn-save "w3-animate-top")
                                                 (write-file (text-value ace) fname :clog-obj obj)
                                                 (set-is-dirty nil)
                                                 (setf last-date (file-write-date fname))
                                                 (sleep .5)
                                                 (remove-class btn-save "w3-animate-top"))
                                               :initial-filename file-name)))
                       (t
                         (cond ((or (not (probe-file file-name))
                                    (eql last-date (file-write-date file-name)))
                                 (add-class btn-save "w3-animate-top")
                                 (write-file (text-value ace) file-name :clog-obj obj)
                                 (set-is-dirty nil)
                                 (setf last-date (file-write-date file-name))
                                 (sleep .5)
                                 (remove-class btn-save "w3-animate-top"))
                               (t
                                 (confirm-dialog obj "File changed on file system. Save?"
                                                 (lambda (result)
                                                   (when result
                                                     (add-class btn-save "w3-animate-top")
                                                     (write-file (text-value ace) file-name :clog-obj obj)
                                                     (set-is-dirty nil)
                                                     (setf last-date (file-write-date file-name))
                                                     (sleep .5)
                                                     (remove-class btn-save "w3-animate-top"))))))))))
          (when m-emacs
            (set-on-click m-emacs (lambda (obj)
                                    (when is-dirty
                                      (save obj nil))
                                    (swank:ed-in-emacs file-name)
                                    (window-close win))))
          (when m-ntab
            (set-on-click m-ntab (lambda (obj)
                                   (when is-dirty
                                     (save obj nil))
                                   (window-close win)
                                   (on-open-file-ext obj :open-file file-name))))
          (set-on-window-can-close win
                                   (lambda (obj)
                                     (cond (is-dirty
                                             (confirm-dialog obj "Save File?"
                                                             (lambda (result)
                                                               (set-is-dirty nil)
                                                               (when result
                                                                 (save obj nil))
                                                               (window-close win))
                                                             :ok-text "Yes" :cancel-text "No")
                                             nil)
                                           (t
                                             t))))
          (set-on-mouse-click btn-save (lambda (obj data)
                                         (save obj data)))
          (set-on-click m-saveas (lambda (obj)
                                   (save obj nil :save-as t)))
          (set-on-click m-save (lambda (obj)
                                 (save obj nil))))
        (labels ((buf-add ()
                   (let ((val (clog-ace:selected-text ace)))
                     (unless (equal val "")
                       (place-inside-top-of (window-content (copy-history-win app))
                                            (create-text-area (window-content (copy-history-win app))
                                                              :class "w3-input"
                                                              :value val
                                                              :auto-place nil)))))
                 (copy ()
                   (buf-add)
                   (clog-ace:clipboard-copy ace))
                 (cut ()
                   (buf-add)
                   (clog-ace:clipboard-cut ace)))
          (set-on-click btn-copy (lambda (obj)
                                   (declare (ignore obj))
                                   (copy)))
          (set-on-click m-copy (lambda (obj)
                                 (declare (ignore obj))
                                 (copy)))
          (set-on-click btn-cut (lambda (obj)
                                  (declare (ignore obj))
                                  (cut)))
          (set-on-click m-cut (lambda (obj)
                                (declare (ignore obj))
                                (cut))))
        (set-on-click btn-paste (lambda (obj)
                                  (declare (ignore obj))
                                  (clog-ace:clipboard-paste ace)))
        (set-on-click m-paste (lambda (obj)
                                (declare (ignore obj))
                                (clog-ace:clipboard-paste ace)))
        (set-on-click btn-del (lambda (obj)
                                (declare (ignore obj))
                                (clog-ace:execute-command ace "del")))
        (set-on-click m-del (lambda (obj)
                              (declare (ignore obj))
                              (clog-ace:execute-command ace "del")))
        (set-on-click btn-undo (lambda (obj)
                                 (declare (ignore obj))
                                 (clog-ace:execute-command ace "undo")))
        (set-on-click m-undo (lambda (obj)
                               (declare (ignore obj))
                               (clog-ace:execute-command ace "undo")))
        (set-on-click btn-redo (lambda (obj)
                                 (declare (ignore obj))
                                 (clog-ace:execute-command ace "redo")))
        (set-on-click m-redo (lambda (obj)
                               (declare (ignore obj))
                               (clog-ace:execute-command ace "redo")))
        (set-on-click m-desc (lambda (obj)
                               (let ((r (make-array '(0) :element-type 'base-char
                                                    :fill-pointer 0 :adjustable t)))
                                 (with-output-to-string (s r)
                                   (let ((*standard-output* s))
                                     (describe (find-symbol (string-upcase (clog-ace:selected-text ace))
                                                            (string-upcase (text-value pac-line)))))
                                   (on-open-file obj :title-class "w3-purple" :title "describe selection"
                                                 :text r)))))
        (set-on-click m-apro (lambda (obj)
                               (let ((r (make-array '(0) :element-type 'base-char
                                                    :fill-pointer 0 :adjustable t)))
                                 (with-output-to-string (s r)
                                   (let ((*standard-output* s))
                                     (apropos (clog-ace:selected-text ace)))
                                   (on-open-file obj :title-class "w3-purple" :title "apropos selection"
                                                 :text r)))))
        (set-on-click m-brws (lambda (obj)
                               (declare (ignore obj))
                               (on-new-sys-browser ace :search (clog-ace:selected-text ace))))
        (set-on-click btn-brws (lambda (obj)
                                 (declare (ignore obj))
                                 (clog-ace:execute-command ace "find-definition")))
        (set-on-click m-brwsp (lambda (obj)
                                (declare (ignore obj))
                                (clog-ace:execute-command ace "find-definition")))
        (set-on-click m-doc (lambda (obj)
                              (open-window (window (connection-body obj))
                                           (format nil "http://l1sp.org/search?q=~A"
                                                   (clog-ace:selected-text ace)))))
        (set-on-click m-pprt (lambda (obj)
                               (declare (ignore obj))
                               (clog-ace:execute-command ace "adjust-tabs")))
        (set-on-click m-ppr (lambda (obj)
                              (declare (ignore obj))
                              (let ((r (make-array '(0) :element-type 'base-char
                                                   :fill-pointer 0 :adjustable t)))
                                (with-output-to-string (s r)
                                  (with-input-from-string (n (text-value ace))
                                    (let ((*standard-output* s))
                                      (indentify:indentify n))))
                                (setf (text-value ace) r)
                                (set-is-dirty t))))
        (set-on-click m-pprs (lambda (obj)
                               (declare (ignore obj))
                               (let ((r (make-array '(0) :element-type 'base-char
                                                    :fill-pointer 0 :adjustable t)))
                                 (with-output-to-string (s r)
                                   (with-input-from-string (n (clog-ace:selected-text ace))
                                     (let ((*standard-output* s))
                                       (indentify:indentify n))))
                                 (js-execute ace (format nil "~A.insert('~A',true)"
                                                         (clog-ace::js-ace ace)
                                                         (escape-string r)))
                                 (set-is-dirty t))))
        (set-on-event-with-data ace "clog-adjust-tabs"
                                (lambda (obj data)
                                  (declare (ignore obj))
                                  (unless (equal data "")
                                    (setf data (format nil "~A;" data))
                                    (let* ((o (clog-ace:selected-text ace))
                                           (p (ppcre:scan "\\S" o))
                                           (r (make-array '(0) :element-type 'base-char
                                                          :fill-pointer 0 :adjustable t)))
                                      (handler-case
                                          (with-output-to-string (s r)
                                            (with-input-from-string (n data)
                                              (let ((*standard-output* s))
                                                (indentify:indentify n))))
                                        (error ()
                                           nil))
                                      (loop
                                        (multiple-value-bind (start end)
                                                             (ppcre:scan "(^.*)\\n" r)
                                          (unless start
                                            (return))
                                          (setf r (subseq r end))))
                                      (setf r (subseq r 0 (ppcre:scan "\\S" r)))
                                      (when p
                                        (setf o (subseq o (ppcre:scan "\\S" o) (length o)))
                                        (setf r (format nil "~A~A" r o)))
                                      (unless (or (eq r nil)
                                                  (equal r ""))
                                        (js-execute ace (format nil "~A.insert('~A',true)"
                                                                (clog-ace::js-ace ace)
                                                                (escape-string r)))
                                        (set-is-dirty t))))))
        (labels ((eval-form (obj)
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
                                                   :capture-console (not editor-use-console-for-evals)
                                                   :capture-result  (not editor-use-console-for-evals)
                                                   :clog-obj (connection-body obj)
                                                   :eval-in-package (text-value pac-line))))
                         (if editor-use-console-for-evals
                             (on-open-console obj)
                             (on-open-file obj :title-class "w3-blue" :title "form eval" :text result))))))
                 (eval-selection (obj)
                   (let ((val (clog-ace:selected-text ace)))
                     (unless (equal val "")
                       (let ((result (capture-eval val :clog-obj obj
                                                   :capture-console (not editor-use-console-for-evals)
                                                   :capture-result  (not editor-use-console-for-evals)
                                                   :eval-in-package (text-value pac-line))))
                         (if editor-use-console-for-evals
                             (on-open-console obj)
                             (on-open-file obj :title-class "w3-blue" :title "selection eval" :text result))))))
                 (eval-file (obj)
                   (let ((val (text-value ace)))
                     (unless (equal val "")
                       (let ((result (capture-eval val :clog-obj obj
                                                   :capture-console (not editor-use-console-for-evals)
                                                   :capture-result  (not editor-use-console-for-evals)
                                                   :eval-in-package (text-value pac-line))))
                         (if editor-use-console-for-evals
                             (on-open-console obj)
                             (on-open-file obj :title-class "w3-blue" :title "file eval" :text result)))))))
          (set-on-click btn-esel (lambda (obj)
                                   (eval-selection obj)))
          (set-on-click m-esel (lambda (obj)
                                 (eval-selection obj)))
          (set-on-click btn-efrm (lambda (obj)
                                   (eval-form obj)))
          (set-on-click m-efrm (lambda (obj)
                                 (eval-form obj)))
          (set-on-click btn-test (lambda (obj)
                                   (eval-file obj)))
          (set-on-click m-test (lambda (obj)
                                 (eval-file obj))))
        win)))))
