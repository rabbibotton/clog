(in-package :clog-tools)

(defun setup-lisp-ace (editor status &key (package "CLOG-USER"))
  (let ((app (connection-data-item editor "builder-app-data")))
    ;; currently there is only one auto complete event for page
    (unless (auto-complete-configured app)
      (clog-ace:set-on-auto-complete editor
                                     (lambda (obj prefix)
                                       (declare (ignore obj))
                                       (when (current-editor-is-lisp app)
                                         ;; we needed to modify Ace's lisp mode to treat : as part of symbol
                                         ;; otherwise lookups do not consider the symbols package. I did try
                                         ;; using a code method but then the automatic replace was only on the symbol
                                         (let* ((p (when (current-control app)
                                                     (attribute (get-placer (current-control app)) "data-panel-id")))
                                                (s (if (eq (current-editor-is-lisp app) t)
                                                       (if (current-control app)
                                                           (string-upcase (attribute (attach-as-child (current-control app) p)
                                                                                     "data-in-package"))
                                                           "CLOG-USER")
                                                       (current-editor-is-lisp app)))
                                                (l (car (swank:simple-completions prefix s))))
                                           (when (current-control app)
                                             (let ((n (get-control-list app p)))
                                               (when n
                                                 (maphash (lambda (k v)
                                                            (declare (ignore k))
                                                            (let ((name (attribute v "data-clog-name")))
                                                              (push `(:caption ,name :value ,(format nil "(~A panel)" name)
                                                                               :meta "control")
                                                                    l)))
                                                          n)
                                                 (push '(:caption "target" :value "target"
                                                         :meta "builder")
                                                       l)
                                                 (push '(:caption "panel" :value "panel"
                                                         :meta "builder")
                                                       l))))
                                           l)))
                                     :meta "swank"))
    ;; find symbol in sys-browser
    (js-execute editor
                (format nil
                        "~A.commands.addCommand({
    name: 'find-definition',
    bindKey: {win: 'Alt-.',  mac: 'Command-.'},
    exec: function(editor) {
        var row = editor.selection.getCursor().row;
        var column = editor.selection.getCursor().column;
        var c;
        while (column > 0) {
          c=editor.session.getTextRange(new ace.Range(row, column-1, row, column));
          if (c=='(' || c==' ') { break; }
          column--;
        }
        var s=column;
        while (column < 200) {
          c=editor.session.getTextRange(new ace.Range(row, column, row, column+1));
          if (c==')' || c==' ') { break; }
          column++;
        }
        c = editor.session.getTextRange(new ace.Range(row, s, row, column));
        ~A.trigger('clog-find', c);
    },
    readOnly: true,
});"
                        (clog-ace::js-ace editor)
                        (jquery editor)))
    (set-on-event-with-data editor "clog-find"
                            (lambda (obj data)
                              (declare (ignore obj))
                              (when (current-editor-is-lisp app)
                                (on-new-sys-browser editor :search data))))
    ;; setup save key
    (js-execute editor
                (format nil
                        "~A.commands.addCommand({
    name: 'save-ace',
    bindKey: {win: 'Ctrl-s',  mac: 'Command-s'},
    exec: function(editor) {
        ~A.trigger('clog-save-ace');
    },
    readOnly: true,
});"
                        (clog-ace::js-ace editor)
                        (jquery editor)))
    ;; setup adjust tab key
    (when (current-editor-is-lisp app)
      (js-execute editor
                (format nil
                        "~A.commands.addCommand({
    name: 'adjust-tabs',
    bindKey: {win: '~A',  mac: '~A'},
    exec: function(editor) {
        var row = editor.selection.getCursor().row;
        var column = editor.selection.getCursor().column;
        var c;
        var sr=row;
        var bp=0;
        row--;
        column=500;
        while (row > 0) {
          c=editor.session.getTextRange(new ace.Range(row, column-1, row, column));
          if (c==')') { bp++; }
          if (c=='(') { bp--; }
          if (bp < 0) { break; }
          column--;
          if (column < 0) { row--; column=500;}
        }
        c = editor.session.getTextRange(new ace.Range(row, 0, sr, 500));
        editor.session.selection.setSelectionRange(new ace.Range(sr, 0, sr, 500));
        ~A.trigger('clog-adjust-tabs',c);
    },
    readOnly: true,
});"
                        (clog-ace::js-ace editor)
                        (if *editor-use-tab-as-tabbify*
                            "Alt-t|Tab"
                            "Alt-t")
                        (if *editor-use-tab-as-tabbify*
                            "Ctrl-t|Tab"
                            "Ctrl-t")
                        (jquery editor))))
    (set-on-event-with-data editor "clog-adjust-tabs"
                            (lambda (obj data)
                              (declare (ignore obj))
                              (unless (equal data "")
                                (setf data (format nil "~A;" data))
                                (let* ((o (clog-ace:selected-text editor))
                                       (p (ppcre:scan "\\S" o))
                                       (r (make-array '(0) :element-type 'base-char
                                                      :fill-pointer 0 :adjustable t)))
                                  (handler-case
                                      (with-output-to-string (s r)
                                        (with-input-from-string (n data)
                                          (let ((*standard-output* s))
                                            (indentify:indentify n))))
                                    (error ()
                                     nil))
                                  (loop
                                    (multiple-value-bind (start end)
                                                         (ppcre:scan "(^.*)\\n" r)
                                      (unless start
                                        (return))
                                      (setf r (subseq r end))))
                                  (setf r (subseq r 0 (ppcre:scan "\\S" r)))
                                  (when p
                                    (setf o (subseq o (ppcre:scan "\\S" o) (length o)))
                                    (setf r (format nil "~A~A" r o)))
                                  (unless (or (eq r nil)
                                              (equal r ""))
                                    (js-execute editor (format nil "~A.insert('~A',true)"
                                                               (clog-ace::js-ace editor)
                                                               (escape-string r))))))))
    ;; find symbol in file search
    (js-execute editor
                (format nil
                        "~A.commands.addCommand({
    name: 'search-definition',
    bindKey: {win: 'Alt-,',  mac: 'Command-,'},
    exec: function(editor) {
        var row = editor.selection.getCursor().row;
        var column = editor.selection.getCursor().column;
        var c;
        while (column > 0) {
          c=editor.session.getTextRange(new ace.Range(row, column-1, row, column));
          if (c=='(' || c==' ') { break; }
          column--;
        }
        var s=column;
        while (column < 200) {
          c=editor.session.getTextRange(new ace.Range(row, column, row, column+1));
          if (c==')' || c==' ') { break; }
          column++;
        }
        c = editor.session.getTextRange(new ace.Range(row, s, row, column));
        ~A.trigger('clog-search', c);
    },
    readOnly: true,
});"
                        (clog-ace::js-ace editor)
                        (jquery editor)))
    (set-on-event-with-data editor "clog-search"
                            (lambda (obj data)
                              (declare (ignore obj))
                              (when (current-editor-is-lisp app)
                                (setf data (ppcre:regex-replace-all "\\" data "\\\\"))
                                (setf data (ppcre:regex-replace-all "\\*" data "\\*"))
                                (setf data (ppcre:regex-replace-all "\\(" data "\\("))
                                (setf data (ppcre:regex-replace-all "\\)" data "\\)"))
                                (setf data (ppcre:regex-replace-all "\\[" data "\\["))
                                (setf data (ppcre:regex-replace-all "\\]" data "\\]"))
                                (setf data (ppcre:regex-replace-all "\\^" data "\\^"))
                                (setf data (ppcre:regex-replace-all "\\$" data "\\$"))
                                (setf data (ppcre:regex-replace-all "\\%" data "\\%"))
                                (on-file-search editor :search data))))
    ;; system browse symbol
    (js-execute editor
                (format nil
                        "~A.commands.addCommand({
    name: 'eval-form',
    bindKey: {win: 'Alt-[',  mac: 'Command-['},
    exec: function(editor) {
        var position = editor.session.doc.positionToIndex (editor.selection.getCursor(), 0);
        ~A.trigger('clog-eval-form', position);
    },
    readOnly: true,
});"
                        (clog-ace::js-ace editor)
                        (jquery editor)))
    (set-on-event-with-data editor "clog-eval-form"
                            (lambda (obj data)
                              (let ((p  (parse-integer data :junk-allowed t))
                                    (tv (text-value editor))
                                    (pk "CLOG-USER")
                                    (lf nil)
                                    (cp 0))
                                (loop
                                  (setf (values lf cp) (read-from-string tv nil nil :start cp))
                                  (unless lf (return nil))
                                  (when (eq (car lf) 'in-package)
                                    (setf pk (second lf)))
                                  (when (> cp p) (return lf)))
                                (when lf
                                  (let ((result (capture-eval lf
                                                              :clog-obj (connection-body editor)
                                                              :eval-in-package (format nil "~A" pk))))
                                    (on-open-file obj :title-class "w3-blue" :title "selection eval"
                                                  :has-time-out *editor-delay-on-eval-sel* :text result))))))
    ;; macroexpand
    (js-execute editor
                (format nil
                        "~A.commands.addCommand({
    name: 'macroexp',
    bindKey: {win: 'Alt-m',  mac: 'Option-m'},
    exec: function(editor) {
        var position = editor.session.doc.positionToIndex (editor.selection.getCursor(), 0);
        ~A.trigger('clog-macroexp', position);
    },
    readOnly: true,
});"
                        (clog-ace::js-ace editor)
                        (jquery editor)))
    (set-on-event-with-data editor "clog-macroexp"
                            (lambda (obj data)
                              (let ((p  (parse-integer data :junk-allowed t))
                                    (tv (text-value editor))
                                    (lf nil)
                                    (cp 0))
                                (loop
                                  (setf (values lf cp) (read-from-string tv nil nil :start cp))
                                  (unless lf (return nil))
                                  (when (> cp p) (return lf)))
                                (let ((result (handler-case
                                                  (prin1-to-string (macroexpand lf))
                                                (error (condition)
                                                  (format nil "Error: ~A" condition)))))
                                  (on-open-file obj :title-class "w3-blue" :title "macroexpand result" :text result)))))
    ;; expand-region
    (js-execute editor
                (format nil
                        "~A.commands.addCommand({
    name: 'expand-region',
    bindKey: {win: 'Alt-=',  mac: 'Control-='},
    exec: function(editor) {
        var currentRange = editor.selection.getAllRanges()[0];
        var start = editor.session.doc.positionToIndex(currentRange.start);
        var end = editor.session.doc.positionToIndex(currentRange.end);
        var positions = '(' + start + ' ' + end + ')'
        ~A.trigger('clog-expand-region', positions);
    },
    readOnly: true,
});"
                        (clog-ace::js-ace editor)
                        (jquery editor)))
    (set-on-event-with-data editor "clog-expand-region"
                            (lambda (obj data)
                              (declare (ignore obj))
                              (let* ((positions (read-from-string data))
                                     (new-region
                                       (judge-expand-region (text-value editor)
                                                            (car positions)
                                                            (cadr positions))))
                                (js-execute editor
                                            (format nil
                                                    "var startIndex = ~A;
var endIndex = ~A;
var startRange = ~A.session.doc.indexToPosition(startIndex);
var endRange = ~:*~A.session.doc.indexToPosition(endIndex);
~:*~A.selection.setRange(new ace.Range(startRange.row, startRange.column, endRange.row, endRange.column));"
                                                    (car new-region)
                                                    (cdr new-region)
                                                    (clog-ace::js-ace editor))))))

    (set-on-change editor
                   (lambda (obj)
                     (let ((s (js-query obj (format nil
                                                    "var row = ~A.selection.getCursor().row; ~
                            var column = ~A.selection.getCursor().column; ~
                            var o = column;
                            var c; var charRange; var b=0; ~
                            while (column > 0) {
                              column--;
                              charRange = new ace.Range(row, column-1, row, column); ~
                              c = ~A.session.getTextRange(charRange); ~
                              if (c==')') { b++ } ~
                              if (c=='(' && b==0) { ~
                                charRange = new ace.Range(row, column, row, o); column=0;~
                                c = ~A.session.getTextRange(charRange);} ~
                              if (c=='(' && b > 0) { b-- } }~
                            c"
                                                    (clog-ace::js-ace obj)
                                                    (clog-ace::js-ace obj)
                                                    (clog-ace::js-ace obj)
                                                    (clog-ace::js-ace obj)))))
                       (unless (equal s "")
                         (with-input-from-string (i s)
                           (ignore-errors
                            (let* (;(pac                       (if (or (eq (current-editor-is-lisp app) t)
                                   ;                                   (eq (current-editor-is-lisp app) nil))
                                   ;                               "CLOG-USER"
                                   (pac                           (string-upcase (or (current-editor-is-lisp app)
                                                                                      package)))
                                   (m                         (read i))
                                   ;(*PACKAGE*                 (find-package pac))
                                   ;(SWANK::*buffer-package*   (find-package pac))
                                   ;(SWANK::*buffer-readtable* *readtable*)
                                   (ms                        (format nil "~A" m))
                                   r)
                              ;(ignore-errors
                               ;(setf r (swank::autodoc `(,ms swank::%CURSOR-MARKER%))))
                              ;(if r
                                  ;(setf r (car r))
                                  (setf r (swank:operator-arglist ms pac)) ;)
                              (when status
                                (setf (advisory-title status) (documentation (find-symbol ms) 'function)))
                              (when r
                                (when status
                                  (setf (text status) (string-downcase r)))))))))))
    (clog-ace:set-auto-completion editor t)
    (setf (clog-ace:theme editor) *editor-theme*)
    (setf (clog-ace:tab-size editor) *editor-tab-size*)
    (js-execute editor
                (format nil "~A.setKeyboardHandler('~A')"
                        (clog-ace::js-ace editor)
                        *editor-keybinding*))
    (js-execute editor
                (format nil "~A.setOptions({~A})"
                        (clog-ace::js-ace editor)
                        *editor-renderer-options*))))

(defun get-package-from-string (c)
  "Determine the currect package based on src contained in string C"
  (when (typep c 'string)
    (with-input-from-string (ins c)
      (loop
        (let ((form (read ins nil)))
          (unless form (return "clog-user"))
          (unless (consp form) (return "clog-user"))
          (when (eq (car form) 'in-package)
            (return (string-downcase (second form)))))))))

;; Expand region
(defun scan-exps (text)
  "Scan all expressions (and strings) in the text, return a list of start-end cons.
It parse the string TEXT without using READ functions."
  (let ((char-count 0)
        (backslash 0)
        exps in-dquotes-p left-dquote left-braces left-brackets)
    (loop for c across text do
      (if (= backslash 0)               ;current char isn't after a backslash
          (if (eql c #\\)
              (incf backslash)          ;if it is a backslash, mark for the next word
              (if (eql c #\")           ;if it is double quote,
                  (if in-dquotes-p      ;end the last string or start a new string
                      (progn (setf in-dquotes-p nil)
                             (push (cons left-dquote (1+ char-count))
                                   exps))
                      (setf in-dquotes-p t
                            left-dquote char-count))
                  (if (not in-dquotes-p) ;if it isn't double quote,
                      (case c            ;check if it's braces
                        (#\( (push char-count left-braces)) ;mark a new pair
                        (#\) (if left-braces                ;end a pair
                                 (push (cons (pop left-braces) (1+ char-count))
                                       exps)))
                        (#\[ (push char-count left-brackets))
                        (#\] (if left-brackets
                                 (push (cons (pop left-brackets) (1+ char-count))
                                       exps)))))))
          (decf backslash))
      (incf char-count))
    exps))

(defun judge-expand-region (text start end)
  "Judge the next wider region to expand to."
  (declare (string text) (number start) (number end))
  (let ((selected (subseq text start end)))
    (or (let ((word-range               ;expand to current word
                (ignore-errors
                 (let* ((edge-scanner (ppcre:create-scanner "[^\\w]")))
                   (if (not (ppcre:scan edge-scanner selected)) ;there isn't word edge in selected
                       (cons (- start   ;search for previous word edge
                                (or (car (ppcre:all-matches
                                          edge-scanner
                                          (reverse (subseq text 0 start))))
                                    start)) ;if nothing, mark from beginning to end.
                             (+ end         ;search for next word edge
                                (or (car (ppcre:all-matches edge-scanner
                                                            (subseq text end)))
                                    (- (length text) end)))))))))
          (if (not (equal word-range (cons start end)))
              word-range))              ;return if it isn't same with selected
        (let ((symbol-range             ;expand to current symbol
                ;; just like expand to word, but search for blanks, braces and double quote.
                (ignore-errors
                 (let* ((edge-scanner (ppcre:create-scanner "[\\s\\(\\)\\[\\]\"]")))
                   (if (not (ppcre:scan edge-scanner selected))
                       (cons (- start
                                (or (car (ppcre:all-matches edge-scanner
                                                            (reverse (subseq text 0 start))))
                                    start))
                             (+ end
                                (or (car (ppcre:all-matches edge-scanner
                                                            (subseq text end)))
                                    (- (length text) end)))))))))
          (if (not (equal symbol-range (cons start end)))
              symbol-range))
        (alexandria:if-let              ;expand to curren expression/string
            ((sexp (ignore-errors
                    (car (sort (delete nil
                                       (mapcar ;find wider expressions contained selected
                                               #'(lambda (pair)
                                                   (if (or (and (< (car pair) start)
                                                                (> (cdr pair) end))
                                                           (and (= (car pair) start)
                                                                (> (cdr pair) end))
                                                           (and (< (car pair) start)
                                                                (= (cdr pair) end)))
                                                       pair))
                                               (scan-exps text)))
                               #'(lambda (obj1 obj2) ;sort it to find the smallest
                                   (> (car obj1) (car obj2))))))))
          (if (or (= (car sexp) start)  ;judge "inner" or "outer" to select
                  (= (cdr sexp) end)
                  (and (= (1+ (car sexp)) start)
                       (= (1- (cdr sexp)) end)))
              sexp
              (cons (1+ (car sexp)) (1- (cdr sexp))))
          (cons start end)))))          ;if no expressions, select all
