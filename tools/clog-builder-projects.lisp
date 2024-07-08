(in-package :clog-tools)

(defun on-show-project (obj &key project)
  (let ((app (connection-data-item obj "builder-app-data")))
    (when project
      (setf (current-project app) project))
    (if (project-win app)
        (window-focus (project-win app))
        (let* ((*default-title-class*      *builder-title-class*)
               (*default-border-class*     *builder-border-class*)
               (win (create-gui-window obj :title "ASD Project Window"
                                           :top 60 :left 325
                                           :width 643 :height 625
                                           :has-pinner t
                                           :keep-on-top t
                                           :client-movement *client-side-movement*)))
          (create-projects (window-content win))
          (setf (project-win app) win)
          (set-on-window-close win (lambda (obj)
                                     (declare (ignore obj))
                                     (setf (project-win app) nil)))))))

(defun projects-load (fname)
  (funcall (read-from-string "asdf:load-system") fname))

(defun projects-list-local-systems ()
  (if *no-quicklisp*
      (list *start-project*)
      (funcall (read-from-string "ql:list-local-systems"))))

(defun projects-local-directories ()
  (if *no-quicklisp*
      nil
      (symbol-value (read-from-string "ql:*local-project-directories*"))))

(defun projects-setup (panel)
  (let* ((app (connection-data-item panel "builder-app-data")))
    (when *open-external*
      (setf (checkedp (open-ext panel)) t))
    (when *open-panels-as-popups*
      (setf (checkedp (pop-panel panel)) t))
    (unless *no-quicklisp*
      (when (uiop:directory-exists-p #P"~/common-lisp/")
        (pushnew #P"~/common-lisp/"
                 (symbol-value (read-from-string "ql:*local-project-directories*"))
                 :test #'equalp)))
    (add-select-option (project-list panel) "None" "None")
    (dolist (n (sort (projects-list-local-systems) #'string-lessp))
      (add-select-option (project-list panel) n n))
    (cond((current-project app)
          (setf (text-value (project-list panel)) (current-project app))
          (projects-populate panel))
         (t
          (setf (text-value (project-list panel)) "None")
          (projects-populate panel)))))

(defun projects-edit-asd (panel target)
  (declare (ignore target))
  (let ((sel (text-value (project-list panel))))
    (on-open-file panel :open-file (asdf:system-source-file
                                     (asdf:find-system sel)))))

(defun projects-unload (panel target)
  (declare (ignore target))
  (let ((sel (text-value (project-list panel))))
    (unless (equal sel "None")
      (asdf:clear-system sel)
      (setf (text-value (project-list panel)) "None")
      (projects-populate panel))))

(defun projects-reload (panel target)
  (declare (ignore target))
  (let ((sel (text-value (project-list panel))))
    (unless (equal sel "None")
      (asdf:clear-system sel)
      (projects-populate panel))))

(defun projects-view-dir (panel)
  (let* ((sel (text-value (project-list panel))))
    (if (equal sel "None")
        (on-dir-tree panel)
        (let ((sys (asdf:find-system (format nil "~A" sel))))
          (on-dir-tree panel :dir (asdf:system-source-directory sys))))))

(defun projects-run (panel)
  (let ((app (connection-data-item panel "builder-app-data"))
        (val (text-value (entry-point panel))))
    (unless (equal val "")
      (update-static-root app)
      (setf clog:*clog-debug*
        (lambda (event data)
          (with-clog-debugger (panel
                              :title val
                              :standard-output (stdout app))
           (funcall event data))))
      (capture-eval (format nil "(~A)" val) :clog-obj panel
                                            :capture-console nil
                                            :capture-result nil
                                            :eval-in-package "clog-user")
      (alert-toast panel "Static Root Set"
                   *static-root* :color-class "w3-yellow" :time-out 3))))

(defun projects-entry-point-change (panel)
  (let* ((sys         (text-value (project-list panel)))
         (entry-point (text-value (entry-point panel)))
         (fname       (asdf:system-source-file (asdf:find-system sys)))
         (sys-list    '()))
    (with-open-file (s fname)
      (loop
        (let* ((line (read s nil)))
          (unless line (return))
          (when (equalp (format nil "~A" (second line)) sys)
            (if (getf line :entry-point)
                (setf (getf line :entry-point) entry-point)
                (setf line (append line `(:entry-point ,entry-point)))))
          (push line sys-list))))
    (with-open-file (s fname :direction :output :if-exists :rename)
      (let ((*print-case* :downcase))
        (dolist (n (reverse sys-list))
          (pprint n s))))))

(defun projects-rerender (panel)
  (let ((app (connection-data-item panel "builder-app-data"))
        (sel (text-value (project-list panel))))
    (unless (equal sel "None")
      (let ((sys (asdf:find-system (format nil "~A/tools" sel))))
        (dolist (n (asdf:component-children sys))
          (let ((name      (format nil "~A" (asdf:component-relative-pathname n)))
                (file-name (asdf:component-pathname n)))
            (when (and (> (length name) 5)
                       (equalp (subseq name (- (length name) 5)) ".clog"))
              (let* ((win              (create-gui-window panel :top 40 :left 225
                                                                :width 645 :height 430))
                     (box              (create-panel-box-layout (window-content win)
                                                                :left-width 0 :right-width 0
                                                                :top-height 33 :bottom-height 0))
                     (content          (center-panel box))
                     (panel-id         (html-id content))
                     (render-file-name (format nil "~A~A.lisp"
                                               (directory-namestring file-name)
                                               (pathname-name file-name))))
                (setf-next-id content 1)
                (setf (overflow content) :auto)
                (init-control-list app panel-id)
                (clrhash (get-control-list app panel-id))
                ;; preset in case of empty clog file
                (setf (attribute content "data-clog-name") "empty-clog-file")
                (setf (attribute content "data-clog-type") "clog-data")
                (setf (attribute content "data-in-package") "clog-user")
                (setf (attribute content "data-custom-slots") "")
                (setf (inner-html content)
                      (or (read-file file-name)
                          ""))
                (on-populate-loaded-window content :win win)
                (setf (window-title (parent (parent panel))) (attribute content "data-clog-name"))
                (write-file (render-clog-code content (bottom-panel box))
                            render-file-name)
                (window-close win)
                (format t "~A -> ~A~%" file-name render-file-name)))))))))

(defun projects-populate (panel)
  (let ((app (connection-data-item panel "builder-app-data"))
        (already (asdf:already-loaded-systems))
        (sel (text-value (project-list panel))))
    (setf (window-title (parent (parent panel)))
          (format nil "ASD Project - ~A" sel))
    (reset-control-pallete panel)
    (setf (inner-html (runtime-list panel)) "")
    (setf (inner-html (designtime-list panel)) "")
    (setf (inner-html (runtime-deps panel)) "")
    (setf (inner-html (design-deps panel)) "")
    (setf (text-value (entry-point panel)) "")
    (setf (disabledp (runtime-add-lisp panel)) t)
    (setf (disabledp (runtime-delete panel)) t)
    (setf (disabledp (designtime-add-lisp panel)) t)
    (setf (disabledp (designtime-add-clog panel)) t)
    (setf (disabledp (designtime-delete panel)) t)
    (setf (disabledp (runtime-add-dep panel)) t)
    (setf (disabledp (runtime-del-dep panel)) t)
    (setf (disabledp (design-add-dep panel)) t)
    (setf (disabledp (design-del-dep panel)) t)
    (setf (disabledp (design-plugin panel)) t)
    (setf (disabledp (entry-point panel)) t)
    (setf (current-project app) (if (equal sel "None")
                                    nil
                                    sel))
    (when (current-project app)
      (cond ((member sel already :test #'equalp)
              (let ((fs (asdf:find-system sel)))
                ;; entry point
                (setf (text-value (entry-point panel))
                      (or (asdf/system:component-entry-point fs)
                          ""))
                (setf (current-project-dir app)
                      (asdf:component-pathname
                        fs))
                ;; fill runtime
                (dolist (n (asdf:component-children
                             fs))
                  (let ((name (asdf:component-relative-pathname n))
                        (path (asdf:component-pathname n)))
                    (add-select-option (runtime-list panel) path name)))
                (dolist (n (asdf:system-depends-on fs))
                  (add-select-option (runtime-deps panel) n n)))
              ;; fill designtime
              (handler-case
                  (let ((sys (asdf:find-system (format nil "~A/tools" sel))))
                    (dolist (n (asdf:component-children sys))
                      (let ((name (asdf:component-relative-pathname n))
                            (path (asdf:component-pathname n)))
                        (add-select-option (designtime-list panel) path name)))
                    (dolist (n (asdf:system-depends-on sys))
                      (add-select-option (design-deps panel) n n))
                    (when (member "clog" (asdf:system-defsystem-depends-on sys) :test #'equalp)
                      (setf (disabledp (runtime-add-lisp panel)) nil)
                      (setf (disabledp (runtime-delete panel)) nil)
                      (setf (disabledp (designtime-add-lisp panel)) nil)
                      (setf (disabledp (designtime-add-clog panel)) nil)
                      (setf (disabledp (designtime-delete panel)) nil)
                      (setf (disabledp (runtime-add-dep panel)) nil)
                      (setf (disabledp (runtime-del-dep panel)) nil)
                      (setf (disabledp (design-add-dep panel)) nil)
                      (setf (disabledp (design-del-dep panel)) nil)
                      (setf (disabledp (design-plugin panel)) nil)
                      (setf (disabledp (entry-point panel)) nil)
                      (setf (disabledp (run-button panel)) nil)))
                (t (c)
                  (declare (ignore c))
                  (add-select-option (designtime-list panel) "" "Missing /tools")
                  (add-select-option (design-deps panel) "" "Missing /tools"))))
            (t
              (flet ((load-proj (answer)
                       (cond (answer
                               (handler-case
                                   (progn
                                     (projects-load (format nil "~A/tools" sel))
                                     (update-static-root app))
                                 (error ()
                                   (projects-load sel)))
                               (window-focus (parent (parent panel)))
                               (projects-populate panel))
                             (t
                               (setf (current-project app) nil)
                               (setf (text-value (project-list panel)) "None")))))
                (cond ((eq *app-mode* :batch)
                        (load-proj t)
                        (projects-rerender panel)
                        (clog:shutdown)
                        (uiop:quit))
                      (t
                        (let* ((*default-title-class*      *builder-title-class*)
                               (*default-border-class*     *builder-border-class*))
                          (confirm-dialog panel "Load project?"
                                          (lambda (answer)
                                            (load-proj answer))
                                          :title "System not loaded"))))))))))

(defun projects-add-dep (panel sys)
  (Input-dialog panel "Enter system name:"
                (lambda (result)
                  (when result
                    (add-dep-to-defsystem sys result)
                    (projects-load sys)
                    (projects-populate panel)))
                :height 230)
  (projects-load sys))

(defun projects-add-plugin (panel sys)
  (input-dialog panel (format nil "Enter plugin name (without /tools), ~
                       plugin will be added to the runtime and designtime:")
                (lambda (result)
                  (when result
                    (let* ((s (format nil "~A/tools" sys)))
                      (add-dep-to-defsystem s (format nil "~A/tools" result))
                      (projects-load s))
                    (add-dep-to-defsystem sys result)
                    (projects-load sys)
                    (projects-populate panel)))
                :height 250))

(defun add-dep-to-defsystem (sys file)
  (let ((fname    (asdf:system-source-file (asdf:find-system sys)))
        (sys-list '()))
    (with-open-file (s fname)
      (loop
        (let* ((line (read s nil)))
          (unless line (return))
          (when (equalp (format nil "~A" (second line)) sys)
            (setf (getf line :depends-on)
                  (append (getf line :depends-on) `(,file))))
          (push line sys-list))))
    (with-open-file (s fname :direction :output :if-exists :rename)
      (let ((*print-case* :downcase))
        (dolist (n (reverse sys-list))
          (pprint n s))))))

(defun remove-dep-from-defsystem (sys file)
  (let ((fname    (asdf:system-source-file (asdf:find-system sys)))
        (sys-list '()))
    (with-open-file (s fname)
      (loop
        (let* ((line (read s nil)))
          (unless line (return))
          (when (equalp (format nil "~A" (second line)) sys)
            (let (new-comp)
              (dolist (n (getf line :depends-on))
                (unless (equalp (format nil "~A" n) file)
                  (push n new-comp)))
              (setf (getf line :depends-on) (reverse new-comp))))
          (push line sys-list))))
    (with-open-file (s fname :direction :output :if-exists :rename)
      (let ((*print-case* :downcase))
        (dolist (n (reverse sys-list))
          (pprint n s))))))

(defun projects-add-lisp (panel sys)
  (Input-dialog panel "Enter lisp component name (with out .lisp):"
                (lambda (result)
                  (when result
                    (let ((path (asdf:component-pathname
                                 (asdf:find-system sys))))
                      (write-file "" (format nil "~A~A.lisp"
                                             path result)
                                  :action-if-exists nil)
                      (add-file-to-defsystem sys result :file)
                      (projects-load sys)
                      (projects-populate panel))))
                :height 230)
  (projects-load sys))

(defun projects-add-clog (panel sys)
  (input-dialog panel (format nil "Enter clog component name (with out .clog), ~
                       a lisp component will also be created in the runtime system:")
                (lambda (result)
                  (when result
                    (let* ((s (format nil "~A/tools" sys))
                           (path (asdf:component-pathname
                                  (asdf:find-system s))))
                      (write-file "" (format nil "~A~A.clog"
                                             path result)
                                  :action-if-exists nil)
                      (add-file-to-defsystem s result :clog-file)
                      (projects-load s))
                    (let ((path (asdf:component-pathname
                                 (asdf:find-system sys))))
                      (write-file "" (format nil "~A~A.lisp"
                                             path result)
                                  :action-if-exists nil)
                      (add-file-to-defsystem sys result :file)
                      (projects-load sys)
                      (projects-populate panel))))
                :height 250))

(defun add-file-to-defsystem (system file ftype)
  (let ((fname    (asdf:system-source-file (asdf:find-system system)))
        (sys-list '()))
    (with-open-file (s fname)
      (loop
        (let* ((line (read s nil)))
          (unless line (return))
          (when (equalp (format nil "~A" (second line)) system)
            (setf (getf line :components)
                  (append (getf line :components) `((,ftype ,file)))))
          (push line sys-list))))
    (with-open-file (s fname :direction :output :if-exists :rename)
      (let ((*print-case* :downcase))
        (dolist (n (reverse sys-list))
          (pprint n s))))))

(defun remove-file-from-defsystem (system file ftype)
  (let ((fname    (asdf:system-source-file (asdf:find-system system)))
        (sys-list '()))
    (with-open-file (s fname)
      (loop
        (let* ((line (read s nil)))
          (unless line (return))
          (when (equalp (format nil "~A" (second line)) system)
            (let (new-comp)
              (dolist (n (getf line :components))
                (unless (and (equalp (first n) ftype)
                             (equalp (second n) file))
                  (push n new-comp)))
              (setf (getf line :components) (reverse new-comp))))
          (push line sys-list))))
    (with-open-file (s fname :direction :output :if-exists :rename)
      (let ((*print-case* :downcase))
        (dolist (n (reverse sys-list))
          (pprint n s)))))
  (projects-load system))

(defun open-projects-component (target panel system list)
  (let ((disp (select-text target))
        (item (text-value target)))
    (cond ((equal item "")
           (alert-toast target "Invalid action" "No /tools project" :time-out 1))
          ((equal (subseq item (1- (length item))) "/")
           (setf (inner-html list) "")
           (dolist (n (asdf:component-children
                       (asdf:find-component
                        (asdf:find-system system)
                        (subseq disp 0 (1- (length disp))))))
             (let ((name (asdf:component-relative-pathname n))
                   (path (asdf:component-pathname n)))
               (add-select-option list path name))))
          ((and (> (length item) 5)
                (equalp (subseq item (- (length item) 5)) ".clog"))
           (if (checkedp (open-ext panel))
               (on-new-builder-panel-ext target :open-file item :open-ext (checkedp (pop-panel panel)))
               (on-new-builder-panel target :open-file item :open-ext (checkedp (pop-panel panel)))))
          (t
           (if (checkedp (open-ext panel))
               (on-open-file-ext target :open-file item)
               (on-open-file target :open-file item))))))
