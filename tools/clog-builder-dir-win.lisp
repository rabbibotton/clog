(in-package :clog-tools)

(defun on-setup-dir-win (panel)
  (populate-dir-win panel "./")
  (when *open-external*
    (setf (checkedp (open-file-ext panel)) t))
  (when *open-panels-as-popups*
    (setf (checkedp (pop-clog panel)) t)))

(defun populate-dir-win (panel d)
  (let ((dir (directory-namestring (uiop:truename* d))))
    (setf (current-dir panel) dir)
    ;; Dirs
    (setf (inner-html (folders panel)) "")
    (add-select-option (folders panel)
                       (format nil "~A" dir)
                       (format nil ".  (~A)" dir))
    (unless (or (equalp dir "/") (equalp dir #P"/"))
      (add-select-option (folders panel) (format nil "~A../" dir) ".."))
    (dolist (item (uiop:subdirectories dir))
      (add-select-option (folders panel) item item))
    ;; Files
    (setf (inner-html (files panel)) "")
    (dolist (item (uiop:directory-files (directory-namestring dir)))
      (add-select-option (files panel) item (file-namestring item)))))

(defun on-select-dir-win (panel)
  (let ((item (value (files panel))))
    (unless (equal item "")
      (cond ((and (> (length item) 5)
                  (equal (subseq item (- (length item) 5)) ".clog"))
             (if (checkedp (open-file-ext panel))
                 (on-new-builder-panel-ext panel :open-file item :open-ext (checkedp (pop-clog panel)))
                 (on-new-builder-panel panel :open-file item :open-ext (checkedp (pop-clog panel)))))
            (t
             (if (checkedp (open-file-ext panel))
                 (on-open-file-ext panel :open-file item)
                 (on-open-file panel :open-file item)))))))

(defun on-delete-dir-win (panel)
  (let ((item (value (files panel))))
    (unless (equal item "")
      (confirm-dialog panel (format nil "Delete ~A?" item)
                      (lambda (result)
                        (when result
                          (uiop:delete-file-if-exists item)
                          (populate-dir-win panel (directory-namestring item))))))))

(defun on-new-dir-dir-win (panel)
  (input-dialog panel "Name of new directory?"
                (lambda (result)
                  (when result
                    (ensure-directories-exist (format nil "~A~A/" (current-dir panel) result))
                    (populate-dir-win panel (current-dir panel))))
                :title "New Directory"))

(defun on-delete-dir-dir-win (panel d)
  (let ((dir (directory-namestring (uiop:truename* d))))
    (confirm-dialog panel (format nil "Delete ~A?" dir)
                    (lambda (result)
                      (when result
                        (handler-case
                            (uiop:delete-empty-directory dir)
                          (error ()
                            (alert-toast panel "Directory Delete Failure"
                                         (format nil "Failed to delete ~A, perhaps not empty." dir))))
                        (populate-dir-win panel (current-dir panel)))))))

(defun on-rename-dir-dir-win (panel d)
  (input-dialog panel "Rename directory to?"
                (lambda (result)
                  (when result
                    (rename-file d (format nil "~A~A/" (current-dir panel) result))
                    (populate-dir-win panel (current-dir panel))))
                :title "Rename Directory"))

(defun on-rename-dir-win (panel)
  (let ((item (value (files panel))))
    (unless (equal item "")
      (input-dialog panel "Rename file to?"
                    (lambda (result)
                      (when result
                        (rename-file item (format nil "~A~A" (directory-namestring item) result))
                        (populate-dir-win panel (current-dir panel))))
                    :title "Rename File"))))
