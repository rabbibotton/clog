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

4. Get the 64 bit SQLite DLL from:

   https://www.sqlite.org/download.html

   Double clip the downloaded dll zip and copy the file to C:\Program Files\Git\mingw64\bin

5. Download QuickLisp:

   Download using http://beta.quicklisp.org/quicklisp.lisp

   (assuming for the tutorial it is downloaded to your Downloads
   directory)

6. Install QuickLisp:

   Open Git Bash and run: sbcl

   Use the mouse right click paste or type:

   - At the * prompt from sbcl type: (load "~/Downloads/quicklisp.lisp")
   - At the * prompt from sbcl type: (quicklisp-quickstart:install)
   - At the * prompt from sbcl type: (ql:add-to-init-file)
   - At the * prompt from sbcl type: (ql:quickload :quicklisp-slime-helper)
   - At the * prompt from sbcl type: (quit)

   Run rho emacs with (I would add to path or make a script):

      /c/Program\ Files/rho-emacs/rho

   Use C-x-f and create the file ~/.emacs.d/init.el and add the next three lines
   (The first line is to include custom emacs themese and settings the next two
    are to install slime for sbcl):

```
      (load "~/.emacs.d/.custom")
      (load (expand-file-name "C:/Users/david/quicklisp/slime-helper.el"))
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
