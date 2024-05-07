(in-package :clog-tools)

(defun on-open-repl-console (obj repl)
  (let* ((win (on-open-file obj :title "CLOG REPL Console"
                                :is-console t
                                :editor-use-console-for-evals t)))
    (set-on-window-can-close win (lambda (obj)
                                   (declare (ignore obj))
                                   (window-focus repl)
                                   nil))
    win))

(defun on-repl (obj)
  "Open a REPL"
  (let* ((app (connection-data-item obj "builder-app-data"))
         (*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (win (create-gui-window obj :title "CLOG Builder REPL"
                                 :has-pinner t
                                 :keep-on-top t
                                 :top 40 :left 225
                                 :width 600 :height 400
                                 :client-movement *client-side-movement*))
         (repl (create-clog-builder-repl (window-content win))))
    (when *clog-repl-private-console*
      (let ((pcon (on-open-repl-console obj win)))
        (setf (window-param win) pcon)
        (set-on-window-close win (lambda (obj)
                                   (declare (ignore obj))
                                   (window-close pcon))))
      (window-focus win))
    (setup-lisp-ace (playground repl) (status repl))
    (setf (advisory-title (send-to-repl repl))
          "Click to send the editor contents to the REPL.")
    (flet ((update-pac (obj)
             (declare (ignore obj))
             (setf (current-editor-is-lisp app) (text-value (package-div repl)))))
      (set-on-window-focus win #'update-pac)
      (set-on-change (package-div repl) #'update-pac)
      (update-pac nil))
    (setf (clog-ace:theme (playground repl)) "ace/theme/terminal")
    (set-geometry repl :units "%" :width 100 :height 100)))

(defun repl-on-create (panel target)
  (declare (ignore target))
  (when *clog-repl-open-console-on-start*
    (on-open-console panel)))

(defun repl-on-send-to-repl (panel target)
  (declare (ignore target))
  (let ((cmd (text-value (playground panel))))
    (clog-terminal:echo (terminal panel) cmd)
    (repl-on-commmand panel (terminal panel) cmd)))

(defun repl-on-commmand (panel target data)
  (cond ((or (equalp data ":e")
             (equalp data ":q"))
          (window-close (parent (parent panel))))
        ((equalp data "(clog-builder-repl)")
         (let* ((*default-title-class*      *builder-title-class*)
                (*default-border-class*     *builder-border-class*)
                (win (create-gui-window panel :title "CLOG Builder REPL GUI Window"
                                        :height 400 :width 600
                                        :has-pinner t
                                        :client-movement *client-side-movement*)))
           (setf clog-user::*body* (window-content win))))
        (t
          (setf data (format nil "(let ((tmp (progn ~A)))
                                    (setf /// //) (setf // /) (setf / (list tmp))
                                    (setf *** **) (setf ** *) (setf * tmp))" data))
          (multiple-value-bind (result new-package)
                               (capture-eval data :clog-obj        panel
                                             :private-console-win (when *clog-repl-private-console*
                                                                    (window-param (parent (parent panel))))
                                             :capture-console (not *clog-repl-use-console*)
                                             :capture-result  (not *clog-repl-send-result-to-console*)
                                             :eval-in-package (text-value (package-div panel)))
            (setf (text-value (package-div panel))
                  (string-downcase (package-name new-package)))
            (clog-terminal:echo target result)))))