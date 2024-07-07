Using QLOT with CLOG Builder

QLOT creates a local quicklisp for your project. The following instructions
will show you how to take a CLOG project (or any Common Lisp project) and
convert it for use with qlot.

https://github.com/fukamachi/qlot

NOTE - qlot does not appear to work with Windows

After you have created a CLOG project, copy it to its own projects directory,
do not use ~/common-lisp or other project directory already in use by your
global quicklisp install.

for this example I created ~/projects and then move my CLOG project qtest to
~/projects/qtest

cd ~/projects/qtest

Create a file call qlfile with:

```
dist http://dist.ultralisp.org/
```

(I am using the REPL method for maximum compatability, if you alredy know
qlot you can use the command line tool if you have installed it.)

run sbcl and type:

```
(ql:quickload :qlot)
(setf qlot:*project-root* (uiop:getcwd))
(qlot:init (uiop:getcwd))
(qlot:install)
```

To work on your project using your qlot quicklisp, cd to your dir (example
~/projects/qtest ) and run
sbcl with:

```
(ql:quickload :qlot)
(setf qlot:*project-root* (uiop:getcwd))
(qlot:init (uiop:getcwd))
(pushnew (uiop:getcwd) ql:*local-project-directories* :test #'equalp)
```

You are now in your private qlot world for your project, so to run the CLOG
builder do:

```
(ql:quickload :qtest/tools)
(clog-tools:clog-builder :project :qtest)
```

To update your local quicklisp use (ql:update-all-dist) or in CLOG Builder
Options -> Update CLOG Builder

If you used qlot and or added directly to the qlfile use (qlot:update :name-of-system)

To make things easier create a file say - ql and chmod 775
```
sbcl --eval "(ql:quickload :qlot)" \
     --eval "(setf qlot:*project-root* (uiop:getcwd))" \
     --eval "(qlot:init (uiop:getcwd))" \
     --eval "(pushnew (uiop:getcwd) ql:*local-project-directories* :test #'equalp)" \
     --eval "(ql:quickload :qtest/tools)" \
     --eval "(clog-tools:clog-builder :project :qtest)"
```

