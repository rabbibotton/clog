;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-tree.lisp                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;; CLOG-TREEs are used to display a collapsable tree structure like directory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-tree (clog-div)
  ((tree-root    :accessor tree-root)
   (indent-level :accessor indent-level)
   (content      :accessor content)
   (toggle-state :accessor toggle-state)
   (toggle-func  :accessor toggle-func))
  (:documentation "CLOG-Tree object - a collapsible tree component"))

(defgeneric tree-root (clog-tree)
  (:documentation "Accessor for clog-tree root, create clog-tree-items
on the tree-root or other clog-tree's."))

(defgeneric indent-level (clog-tree)
  (:documentation "Accessor for clog-tree root, create clog-tree-items
on the tree-root or other clog-tree's."))

(defgeneric toggle-state (clog-tree)
  (:documentation "True if node is open."))

(defmethod create-clog-tree ((obj clog-obj) &key (content "")
                                                 (indent-level 0)
                                                 (node-html "&#128193;") ; folder icon
                                                 (on-context-menu nil)
                                                 (fill-function nil)
                                                 (visible t)
                                                 (class nil)
                                                 (html-id nil)
                                                 (auto-place t))
  "Creates a clog-tree node labeled CONTENT with INDENT-LEVEL using NODE-HTML
icon. When FILL-FUNCTION, when clog-tree is visible the FILL-FUNCTION is called
and when not visible (such as clicked to close) the children are destroyed."
  (let* ((new-obj (create-div obj :content (format nil "~A&nbsp;" node-html)
                              :class class
                              :html-id html-id
                              :auto-place auto-place))
         (header  (create-span new-obj :content content)))
    (change-class new-obj 'clog-tree)
    (setf (content new-obj) header)
    (setf (indent-level new-obj) indent-level)
    (setf (tree-root new-obj) (create-span header))
    (dotimes (n indent-level)
      (create-span new-obj :content "&nbsp;&nbsp;" :auto-place :top))
    (flet ((toggle-me ()
             (cond (fill-function
                     (if visible
                         (setf (text (tree-root new-obj)) "")
                         (funcall fill-function new-obj))
                     (setf visible (not visible)))
                   (t
                     (if visible
                         (setf (hiddenp (tree-root new-obj)) t)
                         (setf (hiddenp (tree-root new-obj)) nil))
                     (setf visible (not visible))))
             (setf (toggle-state new-obj) visible)))
      (setf visible (not visible))
      (toggle-me)
      (setf (toggle-func new-obj) #'toggle-me)
      (when on-context-menu
        (set-on-context-menu new-obj (lambda (obj)
                                       (declare (ignore))
                                       (funcall on-context-menu obj))))
      (set-on-click new-obj (lambda (obj)
                              (declare (ignore obj))
                              (toggle-me))
                    :cancel-event t)) ; prevent event bubble up tree
    new-obj))

(defmethod toggle-tree (clog-tree)
  (:documentation "Toggle state of tree node"))

(defmethod toggle-tree ((obj clog-tree))
  (funcall (toggle-func obj)))

(defclass clog-tree-item (clog-div)
  ((tree-item    :accessor tree-item)
   (indent-level :accessor indent-level)
   (content      :accessor content))
  (:documentation "CLOG-tree-item object - a tree list item"))

(defgeneric tree-item (clog-tree-item)
  (:documentation "Accessor for clog-tree-item item."))

(defmethod create-clog-tree-item ((obj clog-obj) &key (content "")
                                                      (indent-level nil)
                                                      (node-html "&#128196;") ; file icon
                                                      (on-click nil)
                                                      (on-context-menu nil)
                                                      (class nil)
                                                      (html-id nil)
                                                      (auto-place t))
   "Creates a clog-tree-item node labeled CONTENT with INDENT-LEVEL using NODE-HTML
icon. If INDENT-LEVEL is nil get parent's INDENT-LEVEL from obj if is a clog-tree."
  (let* ((new-obj (create-div obj :content (format nil "~A&nbsp;" node-html)
                               :class class
                               :html-id html-id
                               :auto-place auto-place))
         (header  (create-span new-obj :content content)))
    (change-class new-obj 'clog-tree-item)
    (setf (content new-obj) header)
    (unless indent-level
      (when (parent obj)
        (when (parent obj)
          (if (typep (parent (parent obj)) 'clog-tree)
              (setf indent-level (1+ (indent-level (parent (parent obj)))))
              (setf indent-level 0)))))
    (dotimes (n indent-level)
      (create-span new-obj :content "&nbsp;&nbsp;" :auto-place :top))
    (setf (indent-level new-obj) indent-level)
    (setf (tree-item new-obj) (create-span header))
    (when on-context-menu
      (set-on-context-menu new-obj (lambda (obj)
                                     (declare (ignore))
                                     (funcall on-context-menu obj))))
    (when on-click
      (set-on-click new-obj (lambda (obj)
                              (declare (ignore))
                              (funcall on-click obj))
                    :cancel-event t))
    new-obj))
