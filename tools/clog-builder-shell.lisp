(in-package :clog-tools)

(defun on-shell (obj &key dir)
  "Open a shell"
  (let* ((*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (win (create-gui-window obj :title (format nil "OS Pseudo Shell - ~A - ~A"
                                                    (uiop:operating-system)
                                                    (uiop:hostname))
                                 :top (menu-bar-height obj)
                                 :left (+ *builder-left-panel-size* 5)
                                 :width 600 :height 400
                                 :client-movement *client-side-movement*)))
    (set-on-click (create-span (window-icon-area win)
                               :content "&rarr;&nbsp;"
                               :auto-place :top)
                  (lambda (obj)
                    (declare (ignore obj))
                    (set-geometry win
                                  :top (menu-bar-height win)
                                  :left *builder-left-panel-size*
                                  :height "" :width ""
                                  :bottom 5 :right 0)
                    (set-on-window-move win nil)
                    (set-on-window-move win (lambda (obj)
                                              (setf (width obj) (width obj))
                                              (setf (height obj) (height obj))))))
    (set-on-click (create-span (window-icon-area win)
                               :content "-&nbsp;"
                               :auto-place :top)
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (hiddenp win) t)))
    (when dir
        (uiop:chdir (uiop:native-namestring dir)))
    (set-geometry (create-clog-builder-shell (window-content win))
                                            :units "%" :width 100 :height 100)))

(defun shell-on-create (panel target)
  (setf (text-value (package-div panel)) (uiop:getcwd))
  (clog-terminal:prompt target "$ "))

(defun shell-on-commmand (panel target data)
  (handler-case
      (cond ((and (> (length data) 3)
                  (equalp (subseq data 0 3) "cd "))
             (let* ((dir (subseq data 3 (length data)))
                    (wc  (when (> (length dir) 2) (char dir 1)))
                    (sw  (char dir 0)))
               (uiop:with-current-directory ((format nil "~A~A"
                                                     (if (or (equal wc #\:)
                                                             (equal sw (uiop:directory-separator-for-host))
                                                             (equal sw  #\~))
                                                         dir
                                                         (format nil "~A~A~A"
                                                                 (text-value (package-div panel))
                                                                 (uiop:directory-separator-for-host)
                                                                 dir))
                                                     (uiop:directory-separator-for-host)))
                 (setf (text-value (package-div panel)) (uiop:getcwd)))))
            ((equalp data "exit")
             (window-close (parent (parent panel))))
            (t
             (uiop:with-current-directory ((text-value (package-div panel)))
               (multiple-value-bind (result new-package new-dir)
                   (capture-eval (format nil "(uiop:run-program \"~A\" :output *standard-output*)(uiop:getcwd)"
                                         (ppcre:regex-replace-all "\"" (ppcre:regex-replace-all "\\" data "\\\\\\")  "\\\""))
                                 :clog-obj            panel
                                 :eval-form           "~A"
                                 :capture-result-form ""
                                 :capture-console     t
                                 :capture-result      nil)
                 (declare (ignore new-package))
                 (setf (text-value (package-div panel)) new-dir)
                 (clog-terminal:echo target result)))))
    (error (c)
      (clog-terminal:echo target (format nil "~A" c)))))
