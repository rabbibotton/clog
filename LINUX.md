## Installing Common Lisp + CLOG Builder on Linux (all platforms)

(For emacs based Linux install - https://lisp-lang.org/learn/getting-started/)

1. Install sbcl, emacs, sqlite and openssh (modify for your OS)

sudo apt-get install sbcl openssh-client libsqlite3-dev

2. Install QuickLisp
```
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
```

3. Run SBCL and install the UltraLisp distro for recent software for quicklisp:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:update-all-dists) ; run newest updates
```

4. Install and run CLOG Builder for a rich GUI Lisp IDE either with:

```
   (ql:quickload :clog/tools)
   (clog-tools:clog-builder)
```

or at the command line:

```
run-builder
```

-  [Learn about CLOG](README.md)
-  [Learn Common-Lisp](LEARN.md)
