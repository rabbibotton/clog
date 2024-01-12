## Installing Common Lisp on Windows 64bit from Scratch

1. Download and install rho-emacs:

   https://gchristensen.github.io/rho-emacs/
   https://github.com/GChristensen/rho-emacs/releases

   When installing choose C:\Users\yourname for the "home folder"

   I like a plain emacs, others like the various default extensions and themes.

2. Install sbcl:

   https://www.sbcl.org/platform-table.html

3. Get GIT 64 bit:

   https://git-scm.com/download/win

   Even if you don't use GIT, it installs the needed ssl files and
   some basic unix tools like bash
   
   You should choose for line endings, checkout as-is, commit unix-style line endings
   sbcl does not handle crlf well in certain situations

   You should also choose to use the MSYS bash shell

5. Get the 64 bit SQLite DLL from:

   https://www.sqlite.org/download.html

   Double clip the downloaded dll zip and copy the contents of the zip file
   to C:\Program Files\Git\mingw64\bin

7. Download QuickLisp:

   Open the Git Bash shell from your windows apps and run

   cd
   curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp

8. Install QuickLisp:

   Continue in the Git Bash shell and run
   ```
   sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
   sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
   ```
   
   Run rho emacs with (I would add to path or make a script):

      /c/Program\ Files/rho-emacs/rho

   Use C-x-f and create the file ~/.emacs.d/init.el

```
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")
```

7. Restart Emacs

   Quit emacs - C-x C-y

   Start again emacs

   /c/Program\ Files/rho-emacs/rho

   Run Slime - M-x slime

7. Install CLOG:

   (ql:quickload :clog)
   (clog:run-demo 1)

   [Learn about CLOG](README.md)
   [Learn Common-Lisp](LEARN.md)
