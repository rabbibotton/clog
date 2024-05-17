## Installing Common Lisp + Emacs + Slime on MacOS (all platforms)

(If you do not want emacs skip steps 1,2,6, and 7 and just use the
 CLOG Builder)

1. Install Emacs for Mac OS from - https://emacsformacosx.com/

2. [optional] Add Emacs to your system's path by creating a
file /etc/paths.d/emacs with:
```
/Applications/Emacs.app/Contents/MacOS
```

3. Install Homebrew - https://brew.sh/
```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

4. Install a Common Lisp compiler, ie. sbcl
```
brew install sbcl
```

5. Install QuickLisp
```
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
```

6. Install Slime
```
sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
```

7. Modify or create ~/.emacs.d/init.el with the lines
```
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
```

8. Install the UltraLisp distro for recent software for quicklisp:

```
sbcl --eval '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)' --eval '(ql:update-all-dists)' --quit
```

9. Install CLOG and run CLOG Builder for a rich GUI Lisp IDE that works with emacs also:

```
sbcl --eval '(ql:quickload :clog/tools)' --eval '(clog-tools:clog-builder)'
```

10. You can now start emacs graphically or if did option 2 from terminal
using emacs (and if desire terminal version use emacs -nw) and then use M-x slime

Tip: If use emacs graphically, you may find it easier to set the
apple command key to also act as a Meta key:

```
(setf ns-command-modifier 'meta)
;; Since we map command to meta let meta ` switch frames
(global-set-key "\M-`" 'other-frame)
```

Tip: If using emacs in the terminal (ie emacs -nw) in terminal
preferances under profiles and under the default profile choose "Use
Option as Meta". I also add the following to my ~/.emacs.d/init.el:

```
;; Selecting with mouse is an emacs selection
(xterm-mouse-mode t)
;; Fix mouse wheel under xterm-mouse-mode
(global-set-key (kbd "<mouse-4>") (kbd "C-p"))
(global-set-key (kbd "<mouse-5>") (kbd "C-n"))
```


-  [Learn about CLOG](README.md)
-  [Learn Common-Lisp](LEARN.md)
