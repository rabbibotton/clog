(in-package :clog-tools)

(defun on-file-search (obj &key dir search doc-maximize)
  "Open file search"
  (let* ((app (connection-data-item obj "builder-app-data"))
         (*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (lisp-file t)
         (win (create-gui-window obj :top (+ (menu-bar-height obj) 20)
                                 :left 20
                                 :width 1040 :height 600
                                 :client-movement *client-side-movement*))
         (panel (create-panel-search (window-content win))))
    (set-on-click (create-span (window-icon-area win)
                               :content "-&nbsp;"
                               :auto-place :top)
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (hiddenp win) t)))
    (set-on-window-size win (lambda (obj)
                              (declare (ignore obj))
                              (clog-ace:resize (preview-ace panel))))
    (setf (place-holder (pac-line panel)) "Current Package")
    (setf (current-editor-is-lisp app) "clog-user")
    (setf (text-value (pac-line panel)) "clog-user")
    (setup-lisp-ace (preview-ace panel) (status-bar panel))
    (setf (text-value (preview-ace panel))
          ";; After search, double click file name to open / single click to preview")
    (set-on-window-focus win
                         (lambda (obj)
                           (declare (ignore obj))
                           (if lisp-file
                               (setf (current-editor-is-lisp app) (text-value (pac-line panel)))
                               (setf (current-editor-is-lisp app) nil))))
    (set-on-input (result-box panel) (lambda (obj)
                                       (let* ((fname (text-value obj))
                                              (regex   (text-value (grep-input panel)))
                                              (c     (read-file fname :report-errors nil)))
                                         (cond ((or (equalp (pathname-type fname) "lisp")
                                                    (equalp (pathname-type fname) "asd"))
                                                 (setf (text-value (pac-line panel)) (get-package-from-string c))
                                                 (setf (current-editor-is-lisp app) (text-value (pac-line panel)))
                                                 (setf lisp-file t)
                                                 (setf (clog-ace:mode (preview-ace panel)) "ace/mode/lisp"))
                                               (t
                                                 (setf lisp-file nil)
                                                 (setf (current-editor-is-lisp app) nil)
                                                 (if (equalp (pathname-type fname) "clog")
                                                     (setf (clog-ace:mode (preview-ace panel)) "ace/mode/html")
                                                     (setf (clog-ace:mode (preview-ace panel))
                                                           (clog-ace:get-mode-from-extension (preview-ace panel) fname)))))
                                         (setf (text-value (preview-ace panel)) c)
                                         (clog-ace:resize (preview-ace panel))
                                         (js-execute obj (format nil "~A.find({regExp:true,needle:'~A',caseSensitive:false})"
                                                                 (clog-ace::js-ace (preview-ace panel)) (escape-string regex)))
                                         (clog-ace:execute-command (preview-ace panel) "find"))
                                       (focus (result-box panel))))
    (flet ((save (obj)
             (let ((fname (text-value (result-box panel))))
               (when (not (equalp fname ""))
                 (write-file (text-value (preview-ace panel)) fname)
                 (alert-toast obj "Save file..." (format nil "Saved ~A" fname) :color-class "w3-green" :time-out 1)))))
      (set-on-click (save-btn panel) #'save)
      (set-on-event  (preview-ace panel) "clog-save-ace" #'save))
    (labels ((eval-form (obj)
               (let ((p  (parse-integer
                           (js-query obj
                                     (format nil "~A.session.doc.positionToIndex (~A.selection.getCursor(), 0);"
                                             (clog-ace::js-ace (preview-ace panel))
                                             (clog-ace::js-ace (preview-ace panel))))
                           :junk-allowed t))
                     (tv (text-value (preview-ace panel)))
                     (lf nil)
                     (cp 0))
                 (loop
                   (setf (values lf cp) (read-from-string tv nil nil :start cp))
                   (unless lf (return nil))
                   (when (> cp p) (return lf)))
                 (when lf
                   (let ((result (capture-eval lf
                                               :capture-console (not *editor-use-console-for-evals*)
                                               :capture-result  (not *editor-use-console-for-evals*)
                                               :clog-obj (connection-body obj)
                                               :eval-in-package (text-value (pac-line panel)))))
                     (if *editor-use-console-for-evals*
                         (on-open-console obj)
                         (on-open-file obj :title-class "w3-blue" :title "form eval"
                                       :has-time-out *editor-delay-on-eval-form* :text result))))))
             (eval-selection (obj)
               (let ((val (clog-ace:selected-text (pac-line panel))))
                 (unless (equal val "")
                   (let ((result (capture-eval val :clog-obj obj
                                               :capture-console (not *editor-use-console-for-evals*)
                                               :capture-result  (not *editor-use-console-for-evals*)
                                               :eval-in-package (text-value (pac-line panel)))))
                     (if *editor-use-console-for-evals*
                         (on-open-console obj)
                         (on-open-file obj :title-class "w3-blue" :title "selection eval"
                                       :has-time-out *editor-delay-on-eval-sel* :text result))))))
             (eval-file (obj)
               (let ((val (text-value (pac-line panel))))
                 (unless (equal val "")
                   (let ((result (capture-eval val :clog-obj obj
                                               :capture-console (not *editor-use-console-for-evals*)
                                               :capture-result  (not *editor-use-console-for-evals*)
                                               :eval-in-package (text-value (pac-line panel)))))
                     (if *editor-use-console-for-evals*
                         (on-open-console obj)
                         (on-open-file obj :title-class "w3-blue" :title "file eval"
                                       :has-time-out *editor-delay-on-eval-file* :text result)))))))
      (set-on-click (eval-sel-btn panel) (lambda (obj)
                                           (eval-selection obj)))
      (set-on-click (eval-form-btn panel) (lambda (obj)
                                            (eval-form obj)))
      (set-on-click (eval-all-btn panel) (lambda (obj)
                                           (eval-file obj))))
    (unless dir
      (setf dir (if (and (current-project-dir app)
                     (not (equal (current-project-dir app) "")))
                    (current-project-dir app)
                    (uiop:getcwd))))
    (setf (text-value (dir-input panel)) dir)
    (panel-search-dir-change panel (dir-input panel))
    (when doc-maximize
      (window-maximize win))
    (when search
      (setf (text-value (grep-input panel)) search)
      (panel-search-on-click panel nil))))

(defun panel-search-dir-change (panel target)
  (setf (window-title (parent (parent panel)))
        (format nil "Search Project Dir ~A" (text-value target))))

(defun panel-search-on-click (panel target)
  (declare (ignore target))
  (destroy-children (result-box panel))
  (let* ((subdirs (checkedp (subdir-check panel)))
         (nregex  (text-value (name-regex-input panel)))
         (sn      (ppcre:create-scanner nregex :case-insensitive-mode t))
         (regex   (text-value (grep-input panel)))
         (s       (ppcre:create-scanner regex :case-insensitive-mode t)))
    (labels ((do-search (dir prefix)
               (dolist (item (uiop:directory-files dir))
                 (let ((fname (format nil "~A" item)))
                   (when (ppcre:scan sn fname)
                     (let ((c (read-file fname :report-errors nil)))
                       (when (and c
                                  (ppcre:scan s c))
                         (let ((li (create-option (result-box panel)
                                                  :content (format nil "~A~A" prefix (file-namestring item))
                                                  :value fname)))
                           (set-on-double-click li (lambda (obj)
                                                     (declare (ignore obj))
                                                     (on-open-file panel :open-file fname
                                                                   :show-find t
                                                                   :regex regex)))))))))
               (when subdirs
                 (dolist (item (uiop:subdirectories dir))
                   (do-search item (format nil "~A~A/" prefix (first (last (pathname-directory item)))))))))
      (do-search (text-value (dir-input panel)) ""))))