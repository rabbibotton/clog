(in-package :clog-tools)

(defparameter *inspectors*
  `((:name "Print to Console"
     :func ,(lambda (symbol title value clog-obj)
              (declare (ignore symbol))
              (on-open-console clog-obj)
              (print title)
              (print value)))
    (:name "Console Inspector"
     :func ,(lambda (symbol title value clog-obj)
              (declare (ignore title value))
              (on-open-console clog-obj)
              (let ((*default-title-class*      *builder-title-class*)
                    (*default-border-class*     *builder-border-class*)
                    (*standard-input* (make-instance 'console-in-stream :clog-obj clog-obj)))
                (inspect symbol))))))

(defun on-probe-panel (obj)
  (let ((app (connection-data-item obj "builder-app-data")))
    (let* ((*default-title-class*      *builder-title-class*)
           (*default-border-class*     *builder-border-class*)
           (win         (create-gui-window obj :title "CLOG Probe Table"
                                           :width 300
                                           :has-pinner t
                                           :keep-on-top t
                                           :client-movement *client-side-movement*)))
      (create-div (window-content win) :style "left:0px;right:0px" :class "w3-tiny w3-center"
                  :content "use CLOG-TOOL:CLOG-BUILDER-PROBE to add probes")
      (setf (probe-win app) win)
      (set-geometry win :top (menu-bar-height win) :left 0 :height "" :bottom 5 :right "")
      (set-on-window-move win (lambda (obj)
                                (setf (height obj) (height obj))))
      (set-on-window-close win (lambda (obj)
                                 (declare (ignore obj))
                                 (setf (probe-win app) nil)))
      (set-on-click (create-span (window-icon-area win) :content "&larr;&nbsp;" :auto-place :top)
                    (lambda (obj)
                      (declare (ignore obj))
                      (set-geometry win :top (menu-bar-height win) :left 0 :height "" :bottom 5 :right ""))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; clog-builder-probe ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro clog-builder-probe (symbol &key clog-body
                                          (title "")
                                          auto-probe)
  "Display symbol's value in Probe Table in Builder, value is changed when OK
pressed. Probe again in auto-probe seconds. If not tile is set, the symbol is
used for title."
  `(let* ((body  (or ,clog-body
                     *clog-debug-instance*))
          (app   (connection-data-item body "builder-app-data"))
          (title (if (equal ,title "")
                     (format nil "~s" ',symbol) 
                     ,title))
          (freq   ,auto-probe)
          probe
          entry)
     (unless (probe-win app)
       (on-probe-panel body))
     (setf probe (create-div (window-content (probe-win app))
                             :style "border-style:solid;overflow:auto;"
                             :class "w3-small"))
     (setf entry (create-div probe
                             :class "w3-small;"))
     (flet ((refresh ()
              (let ((value (format nil "~A" ,symbol)))
                (setf (text entry) (format nil "~A : ~A"
                                           title
                                           value)))))
       (refresh)
       (set-on-click (create-button probe :content "Refresh")
                     (lambda (obj)
                       (declare (ignore obj))
                       (refresh)))
       (set-on-click (create-button probe :content "Change")
                     (lambda (obj)
                       (declare (ignore obj))
                       (input-dialog body (format nil "New value for ~A?" title)
                                     (lambda (result)
                                       (when (and result
                                                  (not (equal result "")))
                                         (setf ,symbol (eval (read-from-string result))))
                                       (refresh)))))
       (set-on-click (create-button probe :content "Inspect")
                     (lambda (obj)
                       (declare (ignore obj))
                       (let* ((menu (create-panel probe
                                                  :left (left probe) :top (top probe)
                                                  :width (width probe)
                                                  :class *builder-window-desktop-class*
                                                  :auto-place :bottom)))
                         (set-on-mouse-leave menu (lambda (obj) (destroy obj)))
                         (mapcar (lambda (inspector)
                                   (set-on-click (create-div menu :content (getf inspector :name) :class *builder-menu-context-item-class*)
                                                 (lambda (obj)
                                                   (declare (ignore obj))
                                                   (destroy menu)
                                                   (funcall (getf inspector :func)
                                                            ,symbol title (format nil "~A" ,symbol)
                                                            body))))
                                 *inspectors*))))
       (set-on-click (create-button probe :content "Remove")
                     (lambda (obj)
                       (declare (ignore obj))
                       (setf (hiddenp probe) t)))
       (when freq
         (bordeaux-threads:make-thread
           (lambda ()
             (loop
               (sleep freq)
               (when (or (not (validp probe))
                         (hiddenp probe)
                         (not (visiblep probe)))
                 (return))
               (refresh)))
         :name (format nil "clog-builder-probe ~A" title))))))

