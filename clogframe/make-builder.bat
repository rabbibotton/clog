sbcl --eval "(ql:quickload :clog/tools)" --eval "(sb-ext:save-lisp-and-die #P\"builder.exe\" :toplevel (lambda () (clog-tools:clog-builder :port 0 :app t :start-browser nil :clogframe t)) :executable t :compression nil)"
