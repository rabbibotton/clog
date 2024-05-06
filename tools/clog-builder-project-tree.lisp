(in-package :clog-tools)

(defun project-tree-select (panel item)
  (unless (equal item "")
    (cond ((and (> (length item) 5)
                (equal (subseq item (- (length item) 5)) ".clog"))
            (if *open-external*
                (on-new-builder-panel-ext panel :open-file item) ;; need ext for both
                (on-new-builder-panel panel :open-file item)))
          (t
            (if *open-external*
                (on-open-file-ext panel :open-file item)
                (progn
                  (let ((win (on-open-file panel :open-file item)))
                    (when *project-tree-sticky-open*
                      (when win
                        (set-geometry win
                                      :top (menu-bar-height win)
                                      :left 300
                                      :height "" :width ""
                                      :bottom 5 :right 0)
                        (clog-ace:resize (window-param win))
                        (set-on-window-move win (lambda (obj)
                                                  (setf (width obj) (width obj))
                                                  (setf (height obj) (height obj)))))))))))))

(defun on-project-tree (obj &key project)
  (let ((app (connection-data-item obj "builder-app-data")))
    (when (uiop:directory-exists-p #P"~/common-lisp/")
      (pushnew #P"~/common-lisp/"
               (symbol-value (read-from-string "ql:*local-project-directories*"))
               :test #'equalp))
    (when project
      (setf (current-project app) project))
    (if (project-tree-win app)
        (window-focus (project-tree-win app))
        (let* ((*default-title-class*      *builder-title-class*)
               (*default-border-class*     *builder-border-class*)
               (win         (create-gui-window obj :title "Project Tree"
                                               :width 300
                                               :has-pinner t
                                               :keep-on-top t
                                               :client-movement *client-side-movement*))
               (projects    (create-select (window-content win)))
               (panel       (create-panel (window-content win) :background-color :silver
                                          :style "text-align:center;"
                                          :class "w3-tiny"
                                          :height 27 :top 30 :left 0 :right 0))
               (load-btn    (create-button panel :content "no project" :style "height:27px;width:72px"))
               (run-btn     (create-button panel :content "run" :style "height:27px;width:72px"))
               (entry-point "")
               (filter-btn (create-button panel :content "filter" :style "height:27px;width:72px"))
               (asd-btn    (create-button panel :content "asd edit" :style "height:27px;width:72px"))
               (tree       (create-panel (window-content win)
                                         :class "w3-small"
                                         :overflow :scroll
                                         :top 60 :bottom 0 :left 0 :right 0)))
          (setf (project-tree-win app) win)
          (set-geometry win :top (menu-bar-height win) :left 0 :height "" :bottom 5 :right "")
          (set-on-click asd-btn (lambda (obj)
                                  (on-show-project obj)))
          (set-on-window-move win (lambda (obj)
                                    (setf (height obj) (height obj))))
          (set-on-window-close win (lambda (obj)
                                     (declare (ignore obj))
                                     (setf (project-tree-win app) nil)))
          (setf (positioning projects) :absolute)
          (set-geometry projects :height 27 :width "100%" :top 0 :left 0 :right 0)
          (set-on-click filter-btn (lambda (obj)
                                     (declare (ignore obj))
                                     (if (equalp (text-value filter-btn)
                                                 "filter")
                                         (setf (text-value filter-btn) "filter off")
                                         (setf (text-value filter-btn) "filter"))))
          (set-on-click run-btn
            (lambda (obj)
              (let* ((*default-title-class*      *builder-title-class*)
                     (*default-border-class*     *builder-border-class*))
                (input-dialog obj "Run form:"
                              (lambda (result)
                                (when result
                                  (setf clog:*clog-debug*
                                        (lambda (event data)
                                          (with-clog-debugger (panel :standard-output (stdout app))
                                                              (funcall event data))))
                                  (capture-eval result
                                                :clog-obj        obj
                                                :capture-console nil
                                                :capture-result  nil
                                                :eval-in-package "clog-user")))
                                :default-value entry-point))))
          (labels ((project-tree-dir-select (node dir)
                     (let ((filter (equalp (text-value filter-btn)
                                           "filter")))
                       (dolist (item (uiop:subdirectories dir))
                         (unless (and (ppcre:scan *project-tree-dir-filter* (format nil "~A" item))
                                      filter)
                           (create-clog-tree (tree-root node)
                                             :fill-function (lambda (obj)
                                                              (project-tree-dir-select obj (format nil "~A" item)))
                                             :indent-level (1+ (indent-level node))
                                             :visible nil
                                             :content (first (last (pathname-directory item))))))
                       (dolist (item (uiop:directory-files (directory-namestring dir)))
                         (unless (and (ppcre:scan *project-tree-file-filter* (file-namestring item))
                                      filter)
                           (create-clog-tree-item (tree-root node)
                                                  :on-click (lambda (obj)
                                                              (project-tree-select obj (format nil "~A" item)))
                                                  :content (file-namestring item))))))
                   (load-proj (sel)
                     (handler-case
                         (projects-load (format nil "~A/tools" sel))
                       (error ()
                              (projects-load sel)))
                              (setf (text-value load-btn) "loaded")
                              (window-focus win))
                   (on-change (obj)
                     (declare (ignore obj))
                     (let* ((sel  (value projects))
                            (root (quicklisp:where-is-system sel))
                            (dir  (directory-namestring (uiop:truename* root))))
                       (cond (root
                               (setf (current-project app) sel)
                               (setf (text-value load-btn) "not loaded")
                               (setf (text tree) "")
                               (create-clog-tree tree
                                                 :fill-function (lambda (obj)
                                                                  (project-tree-dir-select obj dir))
                                                 :node-html "&#129422;"
                                                 :content root)
                               (let ((already (asdf:already-loaded-systems)))
                                 (if (member sel already :test #'equalp)
                                     (setf (text-value load-btn) "loaded")
                                     (let* ((*default-title-class*      *builder-title-class*)
                                            (*default-border-class*     *builder-border-class*))
                                       (setf (text-value load-btn) "loading")
                                       (confirm-dialog win "Load project?"
                                                       (lambda (answer)
                                                         (if answer
                                                             (load-proj sel)
                                                             (setf (text-value load-btn) "not loaded")))
                                                       :title "System not loaded"))))
                               (setf entry-point (format nil "(~A)"
                                                         (or (asdf/system:component-entry-point (asdf:find-system sel))
                                                             ""))))
                             (t
                               (setf entry-point "")
                               (setf (text-value load-btn) "no project"))))))
            (set-on-click load-btn (lambda (obj)
                                     (declare (ignore obj))
                                     (cond ((equalp (text-value load-btn) "loaded")
                                            (asdf:clear-system (value projects))
                                            (setf (text-value load-btn) "not loaded"))
                                           ((equalp (text-value load-btn) "not loaded")
                                            (load-proj (value projects))))))
            (dolist (n (quicklisp:list-local-systems))
              (add-select-option projects n n :selected (equalp n project))
              (when (equalp n project)
                (on-change projects)))
            (add-select-option projects "" "Select Project" :selected (not project))
            (set-on-change projects #'on-change))))))