(in-package :clog-tools)

(defun populate-dir-win (panel d)
  (let ((dir (directory-namestring (uiop:truename* d))))
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
    (cond ((and (> (length item) 5)
                (equal (subseq item (- (length item) 5)) ".clog"))
           (on-new-builder-panel panel :open-file item))
          (t
           (on-open-file panel :open-file item)))))
