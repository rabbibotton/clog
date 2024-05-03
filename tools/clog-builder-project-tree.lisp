(in-package :clog-tools)

(defclass clog-tree (clog-div)
  ((tree-root    :accessor tree-root)
   (indent-level :accessor indent-level))
  (:documentation "CLOG-Tree object - a collapsible tree component"))

(defgeneric tree-root (clog-tree)
  (:documentation "Accessor for clog-tree root, create clog-tree-items
on the tree-root or other clog-tree's."))

(defmethod create-clog-tree ((obj clog-obj) &key (content "")
                                                 (indent-level 0)
                                                 (node-html "&#128193;")
                                                 (fill-function nil)
                                                 (visible t)
                                                 (class nil)
                                                 (html-id nil)
                                                 (auto-place t))
  (let* ((new-obj (create-div obj :content (format nil "~A&nbsp;" node-html)
                              :class class
                              :html-id html-id
                              :auto-place auto-place))
         (header  (create-span new-obj :content content)))
    (change-class new-obj 'clog-tree)
    (setf (indent-level new-obj) indent-level)
    (setf (tree-root new-obj) (create-span header))
    (dotimes (n indent-level)
      (create-span new-obj :content "&nbsp;&nbsp;" :auto-place :top))
    (flet ((toggle-tree ()
             (cond (fill-function
                     (if visible
                         (setf (text (tree-root new-obj)) "")
                         (funcall fill-function new-obj))
                     (setf visible (not visible)))
                   (t
                     (if visible
                         (setf (hiddenp (tree-root new-obj)) t)
                         (setf (hiddenp (tree-root new-obj)) nil))
                     (setf visible (not visible))))))
      (setf visible (not visible))
      (toggle-tree)
      (set-on-mouse-down new-obj
                         (lambda (obj data)
                           (declare (ignore obj data))
                           (toggle-tree))
                         :cancel-event t)) ; prevent event bubble up tree
    new-obj))

(defclass clog-tree-item (clog-div)
  ((tree-item :accessor tree-item)
   (indent-level :accessor indent-level))
  (:documentation "CLOG-tree-item object - a tree list item"))

(defgeneric tree-item (clog-tree-item)
  (:documentation "Accessor for clog-tree-item item."))

(defmethod create-clog-tree-item ((obj clog-obj) &key (content "")
                                                      (indent-level 0)
                                                      (node-html "&#128196;")
                                                      (on-click nil)
                                                      (class nil)
                                                      (html-id nil)
                                                      (auto-place t))
  (let* ((new-obj (create-div obj :content (format nil "~A&nbsp;" node-html)
                               :class class
                               :html-id html-id
                               :auto-place auto-place))
         (header  (create-span new-obj :content content)))
    (change-class new-obj 'clog-tree-item)
    (dotimes (n indent-level)
      (create-span new-obj :content "&nbsp;&nbsp;" :auto-place :top))
    (setf (indent-level new-obj) indent-level)
    (setf (tree-item new-obj) (create-span header))
    (when on-click
      (set-on-mouse-down new-obj (lambda (obj data)
                                   (declare (ignore data))
                                   (funcall on-click obj))
                         :cancel-event t))
    new-obj))

(defun project-tree-select (panel item)
  (unless (equal item "")
    (cond ((and (> (length item) 5)
                (equal (subseq item (- (length item) 5)) ".clog"))
            (if t
                (on-new-builder-panel-ext panel :open-file item) ;; need ext for both
                (on-new-builder-panel panel :open-file item)))
          (t
            (if nil
                (on-open-file-ext panel :open-file item)
                (on-open-file panel :open-file item))))))

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
                           :indent-level (1+ (indent-level panel))
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
