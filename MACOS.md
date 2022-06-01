
## Installing Common Lisp + Emacs + Slime on MacOS (all platforms)

1. Install Emacs for Msc OS from - https://emacsformacosx.com/

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

8. You can now start emacs graphically or if did option 2 from terminal
and then use M-x slime
