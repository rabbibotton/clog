Tutorial 13 is a minimalist project for CLOG

Move the hello-clog directory to ~/common-lisp or another directory
setup for QuickLisp to search. Then excute:

CL-USER> (ql:quickload :hello-clog)
To load "hello-clog":
  Load 1 ASDF system:
    hello-clog
; Loading "hello-clog"
[package hello-clog]
(:HELLO-CLOG)
CL-USER> (hello-clog:start-app)
