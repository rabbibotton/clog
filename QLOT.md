Using QLOT with CLOG Builder

QLOT creates a local quicklisp for your project. The following instructions
will show you how to take a CLOG project (or and Common Lisp project) and
convert it for use woth qlot.

https://github.com/fukamachi/qlot

NOTE - qlot does not seem to work with window

After have created a CLOG project, copy it to its own porjects DIR do not use 
~/common-lisp


for this example I created ~/projects and then move my CLOG project qtest to
~/projects/qtest

So cd to ~/projects/qtest

Create a file call qlfile with:

```
dist http://dist.ultralisp.org/
```

run sbcl and type:

```
(ql:quickload :qlot)
(setf qlot:*project-root* (uiop:getcwd))
(qlot:init (uiop:getcwd))
(qlot:install)
```

To work on your project using your qlot quicklisp cd to your dir (example
~/projects/qtest ) and run
sbcl with:

```
(ql:quickload :qlot)
(setf qlot:*project-root* (uiop:getcwd))
(qlot:init (uiop:getcwd))
(pushnew (uiop:getcwd) ql:*local-project-directories* :test #'equalp)
```

You are now in your private qlot world for your project so to run the CLOG
builder do:

```
(ql:quickload :qtest/tools)
(clog-tools:clog-builder :project :qtest)
```
