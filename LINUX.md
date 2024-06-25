## Installing Common Lisp + CLOG Builder on Linux (all platforms)

(For emacs based Linux install - https://lisp-lang.org/learn/getting-started/)

1. Install sbcl, sqlite and openssh (modify for your OS)

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
sbcl --eval '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)' --eval '(ql:update-all-dists)'
```

4. Install and run CLOG Builder for a rich GUI Lisp IDE with:

```
sbcl --eval '(ql:quickload :clog/tools)' --eval '(clog-tools:clog-builder)'
```

-  [Learn about CLOG](README.md)
-  [Learn Common-Lisp](LEARN.md)

TIPS:
Install "sudo apt install xdg-utils wslu" to install xdg-open so that wsl
uses the windows browser. Also create or add to your %UserProfile% (home dir) a
file .wslconfig with below or networking though nat and even localhost access
is dog slow. (Windows 11 on)
```
[wsl2]
networkingMode=mirrored
```
