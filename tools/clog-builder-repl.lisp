(in-package :clog-tools)

(defun on-repl (obj)
  "Open a REPL"
  (let* ((*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (win (create-gui-window obj :title "CLOG Builder REPL"
                                     :top 40 :left 225
                                     :width 600 :height 400
                                     :client-movement *client-side-movement*)))
    (set-geometry (create-clog-builder-repl (window-content win))
                  :units "%" :width 100 :height 100)))

(defun repl-on-create (panel target)
  (declare (ignore target))
  (when *clog-repl-open-console-on-start*
    (on-open-console panel)))

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
         (multiple-value-bind (result new-package)
           (capture-eval data :clog-obj        panel
                              :capture-console (not *clog-repl-use-console*)
                              :capture-result  (not *clog-repl-send-result-to-console*)
                              :eval-in-package (text-value (package-div panel)))
           (setf (text-value (package-div panel))
                 (string-downcase (package-name new-package)))
           (clog-terminal:echo target result)))))