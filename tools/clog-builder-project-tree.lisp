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

(defun project-tree-dir-select (panel dir)
  (dolist (item (uiop:subdirectories dir))
    (create-clog-tree (tree-root panel)
                      :fill-function (lambda (obj)
                                       (project-tree-dir-select obj (format nil "~A" item)))
                      :indent-level (1+ (indent-level panel))
                      :visible nil
                      :content (first (last (pathname-directory item)))))
  (dolist (item (uiop:directory-files (directory-namestring dir)))
    (create-clog-tree-item (tree-root panel)
                           :on-click (lambda (obj)
                                       (project-tree-select obj (format nil "~A" item)))
                           ; :indent-level (1+ (indent-level panel))
                           :content (file-namestring item))))

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
               (win     (create-gui-window obj :title "Project Tree"
                                           :width 300
                                           :has-pinner t
                                           :client-movement *client-side-movement*))
               (projects (create-select (window-content win)))
               (dir-loc (create-panel (window-content win) :background-color :silver
                                      :height 27 :top 30 :left 0 :right 0))
               (tree    (create-panel (window-content win) :overflow :scroll
                                      :top 60 :bottom 0 :left 0 :right 0)))
          (setf (project-tree-win app) win)
          (set-geometry win :top (menu-bar-height win) :left 0 :height "" :bottom 5 :right "")
          (set-on-window-move win (lambda (obj)
                                    (setf (height obj) (height obj))))
          (set-on-window-close win (lambda (obj)
                                     (declare (ignore obj))
                                     (setf (project-tree-win app) nil)))
          (setf (positioning projects) :absolute)
          (set-geometry projects :height 27 :width "" :top 0 :left 0 :right 0)
          (add-class dir-loc "w3-tiny")
          (add-class tree "w3-small")
          (flet ((on-change (obj)
                   (declare (ignore obj))
                   (let* ((sel  (value projects))
                          (root (quicklisp:where-is-system sel))
                          (dir  (directory-namestring (uiop:truename* root))))
                     (cond (root
                            (setf (text dir-loc) "Not Loaded")
                            (setf (text tree) "")
                            (create-clog-tree tree
                                              :fill-function (lambda (obj)
                                                               (project-tree-dir-select obj dir))
                                              :node-html "&#129422;"
                                              :content root)
                            (let ((already (asdf:already-loaded-systems)))
                              (if (member sel already :test #'equalp)
                                  (setf (text dir-loc) "Loaded")
                                  (flet ((load-proj (answer)
                                           (cond (answer
                                                  (setf (current-project app) sel)
                                                  (handler-case
                                                      (projects-load (format nil "~A/tools" sel))
                                                    (error ()
                                                      (projects-load sel)))
                                                  (setf (text dir-loc) "Loaded")
                                                  (window-focus win))
                                                 (t
                                                  (setf (current-project app) nil)))))
                                    (let* ((*default-title-class*      *builder-title-class*)
                                           (*default-border-class*     *builder-border-class*))
                                      (confirm-dialog win "Load project?"
                                                      (lambda (answer)
                                                        (load-proj answer))
                                                      :title "System not loaded"))))))
                           (t
                            (setf (text dir-loc) ""))))))
            (dolist (n (quicklisp:list-local-systems))
              (add-select-option projects n n :selected (equalp n project))
              (when (equalp n project)
                (on-change projects)))
            (add-select-option projects "" "Select Project" :selected (not project))
            (set-on-change projects #'on-change))))))
