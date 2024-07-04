(in-package :clog-tools)

(defun on-file-search (obj &key (dir ""))
  "Open file search"
  (let* ((*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (win (create-gui-window obj :title (format nil "Search Dir ~A"
                                                    dir)
                                     :width 600 :height 400
                                     :client-movement *client-side-movement*))
         (panel (create-panel-search (window-content win))))
    (setf (text-value (dir-input panel)) dir)))

(defun panel-search-dir-change (panel target)
  (setf (window-title (parent (parent panel)))
        (format nil "Search Project Dir ~A" (text-value target))))

(defun panel-search-on-click (panel target)
  (declare (ignore target))
  (destroy-children (result-box panel))
  (dolist (item (uiop:directory-files (text-value (dir-input panel))))
    (let* ((fname (format nil "~A" item))
           (regex (text-value (grep-input panel)))
           (s     (ppcre:create-scanner regex :case-insensitive-mode t))
           (c (read-file fname)))
      (when (and c
                 (ppcre:scan s c))
        (let ((li (create-option (result-box panel)
                                 :content (file-namestring item))))
          (flet ((do-select ()
                   (on-open-file panel :open-file fname
                                 :show-find t
                                 :regex regex)))
            (set-on-double-click li (lambda (obj)
                                      (declare (ignore obj))
                                      (do-select)))))))))