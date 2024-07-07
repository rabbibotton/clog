## Installing Common Lisp on Windows 64bit from Scratch

(For a simple "one step" non-emacs install see:
   https://github.com/rabbibotton/clog-win64-ez/releases)

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
```
   cd
   curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
```

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

9. Install the UltraLisp distro for recent software for quicklisp:

```
sbcl --eval '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)' --eval '(ql:update-all-dists)' --quit
```

10. Install CLOG and run CLOG Builder for a rich GUI Lisp IDE that works with emacs also:

```
sbcl --eval '(ql:quickload :clog/tools)' --eval '(clog-tools:clog-builder)'
```

11. Restart Emacs

   Quit emacs - C-x C-y

   Start again emacs
   
```
   /c/Program\ Files/rho-emacs/rho
```
   Run Slime - M-x slime



NOTES for Windows:

I have a zip with the needed DLLs should it come up - https://rabbibotton.github.io/clog/clogframe.zip
and clograme.exe for use with clog/clogframe


[Learn about CLOG](README.md)
   
[Learn Common-Lisp](LEARN.md)
