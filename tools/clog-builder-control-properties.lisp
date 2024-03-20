(in-package :clog-tools)

(defun on-show-control-properties-win (obj)
  "Show control properties window"
  (let* ((app (connection-data-item obj "builder-app-data"))
         (is-hidden  nil)
         (auto-mode  nil)
         (panel  (create-panel (connection-body obj) :positioning :fixed
                                                     :width 400
                                                     :top 40
                                                     :right 0 :bottom 0
                                                     :class "w3-border-left"))
         (content (create-panel panel :width 390 :top 0 :right 0 :bottom 0))
         (side-panel (create-panel panel :top 0 :left 0 :bottom 0 :width 10))
         (pin        (create-div side-panel :content "☑" :class "w3-small"))
         (control-list (create-table content)))
    (setf (background-color side-panel) :black)
    (setf (background-color content) :gray)
    (setf (right-panel app) panel)
    (setf (hiddenp (right-panel app)) t)
    (setf (control-properties-win app) content)
    (setf (properties-list app) control-list)
    (set-on-click side-panel (lambda (obj)
                               (declare (ignore obj))
                               (cond (auto-mode
                                      (setf auto-mode nil)
                                      (setf (text-value pin) "☑")
                                      (setf (width panel) "400px")
                                      (setf is-hidden nil))
                                     (t
                                      (setf auto-mode t)
                                      (setf (text-value pin) "☐")
                                      (setf (width panel) "400px")
                                      (setf is-hidden nil)))))
    (set-on-mouse-leave side-panel (lambda (obj)
                                     (declare (ignore obj))
                                     (when auto-mode
                                       (cond (is-hidden
                                              (setf (width panel) "400px")
                                              (setf is-hidden nil))
                                             (t
                                              (setf (width panel) "10px")
                                              (setf is-hidden t))))))
    (setf (overflow content) :auto)
    (setf (positioning control-list) :absolute)
    (set-geometry control-list :left 0 :top 0 :right 0)))

